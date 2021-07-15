{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

module Interpreter
  ( interpretModule
  ) where

import Control.Monad (foldM, join, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson qualified as Aeson
import Data.Align qualified as Align
import Data.Foldable (asum, fold)
import Data.Functor (void)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.These (These(..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Names (Ident(..), Qualified(..))
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString

type Env = Map (Qualified Ident) Value

data Value
  = Object (HashMap Text Value)
  | Array (Vector Value)
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  | Closure Closure
  | Constructor Constructor
  
data Closure = MkClosure
  { closEnv :: Env
  , closArg :: Ident
  , closBody :: CoreFn.Expr ()
  }
  
data Constructor = MkConstructor
  { ctorName :: Qualified (Names.ProperName 'Names.ConstructorName)
  , ctorApplied :: [Value]
  , ctorUnapplied :: [Ident]
  }

fromJSON :: Aeson.Value -> Value
fromJSON (Aeson.Object x) = Object (fmap fromJSON x)
fromJSON (Aeson.Array x) = Array (fmap fromJSON x)
fromJSON (Aeson.String x) = String x
fromJSON (Aeson.Number x) = Number x
fromJSON (Aeson.Bool x) = Bool x
fromJSON Aeson.Null = Null

toJSON :: Value -> Maybe Aeson.Value
toJSON (Object x) = Aeson.Object <$> traverse toJSON x
toJSON (Array x) = Aeson.Array <$> traverse toJSON x
toJSON (String x) = pure (Aeson.String x)
toJSON (Number x) = pure (Aeson.Number x)
toJSON (Bool x) = pure (Aeson.Bool x)
toJSON Null = pure Aeson.Null
toJSON _ = Nothing

interpretModule :: CoreFn.Module ann -> Aeson.Value -> Either String Aeson.Value
interpretModule CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleDecls } input = do
    env <- bind (Just moduleName) Map.empty (fmap void moduleDecls)
    mainFn <- eval env (CoreFn.Var () (Qualified (Just moduleName) (Ident "main")))
    output <- apply mainFn (fromJSON input)
    case toJSON output of
      Nothing -> Left "interpretModule: not JSON output"
      Just result -> pure result
  where
    eval :: Env -> CoreFn.Expr () -> Either String Value
    eval env (CoreFn.Literal _ lit) =
      evalLit env lit
    eval env (CoreFn.Accessor _ pss e) = do
      val <- eval env e
      field <- evalPSString pss
      case val of
        Object o ->
          case HashMap.lookup field o of
            Just x -> pure x
            Nothing -> Left "eval: field not found"
    eval env (CoreFn.Abs _ arg body) =
      pure (Closure (MkClosure env arg body))
    eval env (CoreFn.App _ f x) =
      join (apply <$> eval env f <*> eval env x)
    eval env (CoreFn.Var _ name) =
      case Map.lookup name env of
        Nothing -> Left "eval: unknown ident"
        Just val -> pure val
    eval env (CoreFn.Let _ binders body) = do
      env' <- bind Nothing env binders
      eval env' body
    eval env (CoreFn.ObjectUpdate _ e updates) = do
      val <- eval env e
      let updateOne 
            :: HashMap Text Value
            -> (PSString.PSString, CoreFn.Expr ())
            -> Either String (HashMap Text Value)
          updateOne o (pss, new) = do
            field <- evalPSString pss
            newVal <- eval env new
            pure (HashMap.insert field newVal o)
      case val of
        Object o -> Object <$> foldM updateOne o updates
        _ -> Left "eval: expected object"
    eval env (CoreFn.Case _ args alts) = do
      vals <- traverse (eval env) args
      result <- runMaybeT (asum (map (match vals) alts))
      case result of
        Nothing -> Left "eval: inexhaustive pattern match"
        Just (newEnv, matchedExpr) -> eval (newEnv <> env) matchedExpr
    eval env (CoreFn.Constructor _ _tyName ctor fields) = 
      pure (Constructor (MkConstructor (Qualified (Just moduleName) ctor) [] fields))

    match :: [Value]
          -> CoreFn.CaseAlternative ()
          -> MaybeT (Either String) (Env, CoreFn.Expr ())
    match vals (CoreFn.CaseAlternative binders expr) | length vals == length binders = do
      newEnv <- fold <$> zipWithM matchOne vals binders
      case expr of
        Left guards -> MaybeT (Left "match: guards not supported")
        Right e -> pure (newEnv, e)
    match _ _ = MaybeT (Left "match: incorrect number of arguments")

    matchOne :: Value -> CoreFn.Binder () -> MaybeT (Either String) Env
    matchOne _ (CoreFn.NullBinder _) = pure mempty
    matchOne val (CoreFn.LiteralBinder _ lit) = matchLit val lit
    matchOne val (CoreFn.VarBinder _ ident) = MaybeT do
      pure (Just (Map.singleton (Qualified Nothing ident) val))
    matchOne val (CoreFn.NamedBinder _ ident b) = do
      env <- matchOne val b
      pure (Map.insert (Qualified Nothing ident) val env)
    matchOne (Constructor (MkConstructor ctor vals [])) (CoreFn.ConstructorBinder _ _tyName ctor' bs) 
      | ctor == ctor'
      = fold <$> zipWithM matchOne vals bs
    matchOne Constructor{} CoreFn.ConstructorBinder{} =
      MaybeT (Left "matchOne: unsaturated constructor application")
    matchOne _ _ = MaybeT (pure Nothing)

    matchLit
      :: Value
      -> CoreFn.Literal (CoreFn.Binder ())
      -> MaybeT (Either String) Env
    matchLit (Number n) (CoreFn.NumericLiteral (Left i)) 
      | fromIntegral i == n = pure mempty
    matchLit (Number n) (CoreFn.NumericLiteral (Right d))
      | realToFrac d == n = pure mempty
    matchLit (String s) (CoreFn.StringLiteral pss) = MaybeT do
      s' <- evalPSString pss
      if s' == s
        then pure (Just mempty)
        else pure Nothing
    matchLit (String s) (CoreFn.CharLiteral chr)
      | s == Text.singleton chr = pure mempty
    matchLit (Bool b) (CoreFn.BooleanLiteral b')
      | b == b' = pure mempty
    matchLit (Array xs) (CoreFn.ArrayLiteral bs)
      | length xs == length bs
      = fold <$> zipWithM matchOne (Vector.toList xs) bs
    matchLit (Object o) (CoreFn.ObjectLiteral bs) = do
      vals <- lift (HashMap.fromList <$> traverse (\(pss, b) -> (, b) <$> evalPSString pss) bs)
      let matchField :: These Value (CoreFn.Binder ()) -> MaybeT (Either String) Env
          matchField This{} = pure mempty
          matchField That{} = MaybeT (Left "matchField: field doesn't exist in object")
          matchField (These val b) = matchOne val b
      fold <$> sequence (Align.alignWith matchField o vals)
    matchLit _ _ = MaybeT (pure Nothing)

    evalPSString :: PSString.PSString -> Either String Text
    evalPSString pss = 
      case PSString.decodeString pss of
        Just field -> pure field
        _ -> Left "evalPSString: invalid field name"

    evalLit :: Env -> CoreFn.Literal (CoreFn.Expr ()) -> Either String Value
    evalLit env (CoreFn.NumericLiteral (Left int)) =
      pure (Number (fromIntegral int))
    evalLit env (CoreFn.NumericLiteral (Right dbl)) =
      pure (Number (realToFrac dbl))
    evalLit env (CoreFn.StringLiteral str) =
      String <$> evalPSString str
    evalLit env (CoreFn.CharLiteral chr) =
      pure (String (Text.singleton chr))
    evalLit env (CoreFn.BooleanLiteral b) =
      pure (Bool b)
    evalLit env (CoreFn.ArrayLiteral xs) = do
      vs <- traverse (eval env) xs
      pure (Array (Vector.fromList vs))
    evalLit env (CoreFn.ObjectLiteral xs) = do
      let evalField (pss, e) = do
            field <- evalPSString pss
            val <- eval env e
            pure (field, val)
      vs <- traverse evalField xs
      pure (Object (HashMap.fromList vs))

    bind :: Maybe Names.ModuleName ->  Env -> [CoreFn.Bind ()] -> Either String Env
    bind scope = foldM go where
      go :: Env -> CoreFn.Bind () -> Either String Env
      go env (CoreFn.NonRec _ name e) = do
        val <- eval env e
        pure (Map.insert (Qualified scope name) val env)
      go env (CoreFn.Rec es) = 
        Left "bind: Rec not supported" 

    apply :: Value -> Value -> Either String Value
    apply (Closure MkClosure{ closEnv, closArg, closBody }) arg =
      eval (Map.insert (Qualified Nothing closArg) arg closEnv) closBody
    apply (Constructor MkConstructor{ ctorName, ctorApplied, ctorUnapplied = hd : tl }) arg = do
      pure (Constructor MkConstructor{ ctorName, ctorApplied = arg : ctorApplied, ctorUnapplied = tl })
    apply _ _ = Left "apply: expected closure or unsaturated constructor"
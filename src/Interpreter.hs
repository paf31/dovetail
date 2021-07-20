{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

module Interpreter
  ( Env
  , Value(..)
  , Closure(..)
  , Constructor(..)
  , eval
  , apply
  , interpretModule
  , ToValue(..)
  , FromValue(..)
  , primitive
  , builtIn
  , unsafeClosure
  ) where

import Control.Monad (foldM, join, zipWithM)
import Control.Monad.Supply (evalSupply)
import Control.Monad.Supply.Class (MonadSupply(..))
import Control.Monad.Trans.Cont (ContT, evalContT)
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
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific, floatingOrInteger)
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
  , closBody :: Env -> ContT Value (Either String) Value
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

class ToValue a where
  toValue :: MonadSupply m => m (a -> ContT Value (Either String) Value)
  
  default toValue :: (MonadSupply m, Primitive a) => m (a -> ContT Value (Either String) Value)
  toValue = (pure . ) <$> primitive
  
instance ToValue Value
instance ToValue Integer
instance ToValue Scientific
instance ToValue Text
instance ToValue Bool

instance ToValue a => ToValue (Maybe a) where
  toValue = do
    mk <- toValue
    pure (maybe (pure Null) mk)

instance (e ~ String, ToValue a) => ToValue (Either e a) where
  toValue = do
    mk <- toValue
    pure $ (>>= mk) . lift

instance (r ~ Value, m ~ Either String, ToValue a) => ToValue (ContT r m a) where
  toValue = do
    mk <- toValue
    pure $ (>>= mk)

instance ToValue a => ToValue (Vector a) where
  toValue = do
    mk <- toValue
    pure $ fmap Array . traverse mk

instance (FromValue a, ToValue b) => ToValue (a -> b) where
  toValue = do
    argName <- Names.freshIdent'
    mk <- toValue
    pure $ \f -> pure $ unsafeClosure argName (mk . f)
  
unsafeClosure :: FromValue a => Ident -> (a -> ContT Value (Either String) Value) -> Value
unsafeClosure argName f = Closure $
  MkClosure 
    { closEnv = Map.empty
    , closArg = argName
    , closBody = \env -> do
        case Map.lookup (Qualified Nothing argName) env of
          Nothing -> lift $ Left "toValue: argName not in env"
          Just input -> do
            a <- lift $ fromValue input 
            f a
    }
  
class Primitive a where
  primitive :: MonadSupply m => m (a -> Value)
  
instance Primitive Value where
  primitive = pure id
  
instance Primitive Integer where
  primitive = pure (Number . fromIntegral)
  
instance Primitive Scientific where
  primitive = pure Number
  
instance Primitive Text where
  primitive = pure String
  
instance Primitive Bool where
  primitive = pure Bool
  
instance Primitive a => Primitive (Maybe a) where
  primitive = do
    mk <- primitive
    pure (maybe Null mk)
  
instance (FromValue a, ToValue b) => Primitive (a -> b) where
  primitive = do
    argName <- Names.freshIdent'
    mk <- toValue
    pure $ \f -> unsafeClosure argName (mk . f)
  
builtIn :: Primitive a => Text -> a -> Interpreter.Env
builtIn name value = evalSupply 0 do
  mk <- Interpreter.primitive
  let qualName = Names.mkQualified (Names.Ident name) (Names.ModuleName "Main")
  pure $ Map.singleton qualName $ mk value
  
class FromValue a where
  fromValue :: Value -> Either String a
  
instance FromValue Value where
  fromValue = pure
  
instance FromValue Text where
  fromValue (String s) = pure s
  fromValue _ = Left "fromValue: expected string"
  
instance FromValue Integer where
  fromValue (Number s) 
    | Right i <- floatingOrInteger s = pure i
  fromValue _ = Left "fromValue: expected integer"
  
instance FromValue Scientific where
  fromValue (Number s) = pure s
  fromValue _ = Left "fromValue: expected number"
  
instance FromValue Bool where
  fromValue (Bool b) = pure b
  fromValue _ = Left "fromValue: expected boolean"
  
instance FromValue a => FromValue (Maybe a) where
  fromValue Null = pure Nothing
  fromValue v = Just <$> fromValue v
  
instance FromValue a => FromValue (Vector a) where
  fromValue (Array xs) = traverse fromValue xs
  fromValue _ = Left "fromValue: expected array"
  
interpretModule :: Env -> CoreFn.Module ann -> Aeson.Value -> Either String Aeson.Value
interpretModule initialEnv CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleDecls } input = do
  env <- bind moduleName (Just moduleName) initialEnv (fmap void moduleDecls)
  output <- evalContT do
    mainFn <- eval moduleName env (CoreFn.Var () (Qualified (Just moduleName) (Ident "main")))
    apply mainFn (fromJSON input)
  case toJSON output of
    Nothing -> Left "interpretModule: not JSON output"
    Just result -> pure result

eval :: Names.ModuleName -> Env -> CoreFn.Expr () -> ContT Value (Either String) Value
eval mn env (CoreFn.Literal _ lit) =
  evalLit mn env lit
eval mn env (CoreFn.Accessor _ pss e) = do
  val <- eval mn env e
  field <- lift $ evalPSString pss
  case val of
    Object o ->
      case HashMap.lookup field o of
        Just x -> pure x
        Nothing -> lift $ Left "eval: field not found"
eval mn env (CoreFn.Abs _ arg body) =
  pure (Closure (MkClosure env arg (\env' -> eval mn env' body)))
eval mn env (CoreFn.App _ f x) =
  join (apply <$> eval mn env f <*> eval mn env x)
eval mn env (CoreFn.Var _ name) =
  case Map.lookup name env of
    Nothing -> lift . Left $ "eval: unknown ident " <> show name
    Just val -> pure val
eval mn env (CoreFn.Let _ binders body) = do
  env' <- lift $ bind mn Nothing env binders
  eval mn env' body
eval mn env (CoreFn.ObjectUpdate _ e updates) = do
  val <- eval mn env e
  let updateOne 
        :: HashMap Text Value
        -> (PSString.PSString, CoreFn.Expr ())
        -> ContT Value (Either String) (HashMap Text Value)
      updateOne o (pss, new) = do
        field <- lift $ evalPSString pss
        newVal <- eval mn env new
        pure (HashMap.insert field newVal o)
  case val of
    Object o -> Object <$> foldM updateOne o updates
    _ -> lift $ Left "eval: expected object"
eval mn env (CoreFn.Case _ args alts) = do
  vals <- traverse (eval mn env) args
  result <- runMaybeT (asum (map (match vals) alts))
  case result of
    Nothing -> lift $ Left "eval: inexhaustive pattern match"
    Just (newEnv, matchedExpr) -> eval mn (newEnv <> env) matchedExpr
eval mn env (CoreFn.Constructor _ _tyName ctor fields) = 
  pure (Constructor (MkConstructor (Qualified (Just mn) ctor) [] fields))

match :: [Value]
      -> CoreFn.CaseAlternative ()
      -> MaybeT (ContT Value (Either String)) (Env, CoreFn.Expr ())
match vals (CoreFn.CaseAlternative binders expr) | length vals == length binders = do
  newEnv <- fold <$> zipWithM matchOne vals binders
  case expr of
    Left guards -> lift . lift $ Left "match: guards not supported"
    Right e -> pure (newEnv, e)
match _ _ = lift . lift $ Left "match: incorrect number of arguments"

matchOne :: Value -> CoreFn.Binder () -> MaybeT (ContT Value (Either String)) Env
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
  lift . lift $ Left "matchOne: unsaturated constructor application"
matchOne _ _ = MaybeT (pure Nothing)

matchLit
  :: Value
  -> CoreFn.Literal (CoreFn.Binder ())
  -> MaybeT (ContT Value (Either String)) Env
matchLit (Number n) (CoreFn.NumericLiteral (Left i)) 
  | fromIntegral i == n = pure mempty
matchLit (Number n) (CoreFn.NumericLiteral (Right d))
  | realToFrac d == n = pure mempty
matchLit (String s) (CoreFn.StringLiteral pss) = MaybeT do
  s' <- lift $ evalPSString pss
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
  vals <- lift . lift $ HashMap.fromList <$> traverse (\(pss, b) -> (, b) <$> evalPSString pss) bs
  let matchField :: These Value (CoreFn.Binder ()) -> MaybeT (ContT Value (Either String)) Env
      matchField This{} = pure mempty
      matchField That{} = lift . lift $ Left "matchField: field doesn't exist in object"
      matchField (These val b) = matchOne val b
  fold <$> sequence (Align.alignWith matchField o vals)
matchLit _ _ = MaybeT (pure Nothing)

evalPSString :: PSString.PSString -> Either String Text
evalPSString pss = 
  case PSString.decodeString pss of
    Just field -> pure field
    _ -> Left "evalPSString: invalid field name"

evalLit :: Names.ModuleName -> Env -> CoreFn.Literal (CoreFn.Expr ()) -> ContT Value (Either String) Value
evalLit mn env (CoreFn.NumericLiteral (Left int)) =
  pure (Number (fromIntegral int))
evalLit mn env (CoreFn.NumericLiteral (Right dbl)) =
  pure (Number (realToFrac dbl))
evalLit mn env (CoreFn.StringLiteral str) =
  String <$> lift (evalPSString str)
evalLit mn env (CoreFn.CharLiteral chr) =
  pure (String (Text.singleton chr))
evalLit mn env (CoreFn.BooleanLiteral b) =
  pure (Bool b)
evalLit mn env (CoreFn.ArrayLiteral xs) = do
  vs <- traverse (eval mn env) xs
  pure (Array (Vector.fromList vs))
evalLit mn env (CoreFn.ObjectLiteral xs) = do
  let evalField (pss, e) = do
        field <- lift $ evalPSString pss
        val <- eval mn env e
        pure (field, val)
  vs <- traverse evalField xs
  pure (Object (HashMap.fromList vs))

bind :: Names.ModuleName -> Maybe Names.ModuleName ->  Env -> [CoreFn.Bind ()] -> Either String Env
bind mn scope = foldM go where
  go :: Env -> CoreFn.Bind () -> Either String Env
  go env (CoreFn.NonRec _ name e) = do
    val <- evalContT (eval mn env e)
    pure (Map.insert (Qualified scope name) val env)
  go env (CoreFn.Rec es) = 
    Left "bind: Rec not supported" 

apply :: Value -> Value -> ContT Value (Either String) Value
apply (Closure MkClosure{ closEnv, closArg, closBody }) arg =
  closBody (Map.insert (Qualified Nothing closArg) arg closEnv)
apply (Constructor MkConstructor{ ctorName, ctorApplied, ctorUnapplied = hd : tl }) arg = do
  pure (Constructor MkConstructor{ ctorName, ctorApplied = arg : ctorApplied, ctorUnapplied = tl })
apply _ _ = lift $ Left "apply: expected closure or unsaturated constructor"
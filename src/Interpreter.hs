{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Interpreter
  ( Env
  , Value(..)
  , Closure(..)
  , Constructor(..)
  , EvalException(..)
  , eval
  , apply
  , interpret
  , ToValue(..)
  , FromValue(..)
  , builtIn
  ) where

import Control.Exception (Exception, evaluate, throw, try)
import Control.Monad (guard, foldM, join, zipWithM)
import Control.Monad.Supply (evalSupply)
import Control.Monad.Supply.Class (MonadSupply(..))
import Control.Monad.Trans.Class (lift)
import Data.Align qualified as Align
import Data.Foldable (asum, fold)
import Data.Functor (void)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.These (These(..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics qualified as G
import GHC.TypeLits (KnownSymbol, symbolVal)
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
  , closBody :: Env -> Value
  }
  
data Constructor = MkConstructor
  { ctorName :: Qualified (Names.ProperName 'Names.ConstructorName)
  , ctorApplied :: [Value]
  , ctorUnapplied :: [Ident]
  }
  
data EvalException 
  = UnknownIdent (Qualified Ident)
  | TypeMismatch Text
  | FieldNotFound Text
  | InexhaustivePatternMatch
  | NotSupported Text 
  -- ^ TODO: remove this
  | InvalidNumberOfArguments Int Int
  | UnsaturatedConstructorApplication
  | InvalidFieldName PSString.PSString
  | OtherError Text
  deriving Show

instance Exception EvalException
  
unsafeClosure :: Ident -> (Value -> Value) -> Value
unsafeClosure argName f = Closure $
  MkClosure 
    { closEnv = Map.empty
    , closArg = argName
    , closBody = \env -> do
        case Map.lookup (Qualified Nothing argName) env of
          Nothing -> throw (UnknownIdent (Qualified Nothing argName))
          Just input -> f input
    }
  
class ToValue a where
  toValue :: MonadSupply m => m (a -> Value)
  
  default toValue :: (MonadSupply m, G.Generic a, GToObject (G.Rep a)) => m (a -> Value)
  toValue = ((Object .) . (. G.from)) <$> gToObject
  
instance ToValue Value where
  toValue = pure id
  
instance ToValue Integer where
  toValue = pure (Number . fromIntegral)
  
instance ToValue Scientific where
  toValue = pure Number
  
instance ToValue Text where
  toValue = pure String
  
instance ToValue Bool where
  toValue = pure Bool
  
instance (FromValue a, ToValue b) => ToValue (a -> b) where
  toValue = do
    argName <- Names.freshIdent'
    toA <- fromValue
    fromB <- toValue
    pure $ \f -> unsafeClosure argName (fromB . f . toA)

instance ToValue a => ToValue (Maybe a) where
  toValue = do
    mk <- toValue
    pure (maybe Null mk)

instance ToValue a => ToValue (Vector a) where
  toValue = do
    mk <- toValue
    pure $ Array . fmap mk
  
class FromValue a where
  fromValue :: MonadSupply m => m (Value -> a)
  
  default fromValue :: (MonadSupply m, G.Generic a, GFromObject (G.Rep a)) => m (Value -> a)
  fromValue = do
    mk <- gFromObject
    pure \case
      Object o -> G.to (mk o)
      _ -> throw (TypeMismatch "object")
  
instance FromValue Value where
  fromValue = pure id
  
instance FromValue Text where
  fromValue = pure \case
    String s -> s
    _ -> throw (TypeMismatch "string")
  
instance FromValue Integer where
  fromValue = pure \case
    Number s
      | Right i <- floatingOrInteger s -> i
    _ -> throw (TypeMismatch "integer")
  
instance FromValue Scientific where
  fromValue = pure \case
    Number s -> s
    _ -> throw (TypeMismatch "number")
  
instance FromValue Bool where
  fromValue = pure \case
    Bool b -> b
    _ -> throw (TypeMismatch "boolean")
  
instance FromValue a => FromValue (Maybe a) where
  fromValue  = do
    fromA <- fromValue
    pure $ \case
      Null -> Nothing
      v -> Just (fromA v)
  
instance FromValue a => FromValue (Vector a) where
  fromValue = do
    fromA <- fromValue
    pure \case
      Array xs -> fmap fromA xs
      _ -> throw (TypeMismatch "array")
  
instance (ToValue a, FromValue b) => FromValue (a -> b) where
  fromValue = do
    fromA <- toValue
    toB <- fromValue
    pure $ \f a -> 
      let result = apply f (fromA a)
       in toB result
       
class GToObject f where
  gToObject :: MonadSupply m => m (f x -> HashMap Text Value)
  
instance GToObject f => GToObject (G.M1 G.D t f) where
  gToObject = (. G.unM1) <$> gToObject
  
instance GToObject f => GToObject (G.M1 G.C t f) where
  gToObject = (. G.unM1) <$> gToObject
  
instance (GToObject f, GToObject g) => GToObject (f G.:*: g) where
  gToObject = do
    mk1 <- gToObject
    mk2 <- gToObject
    pure \(f G.:*: g) -> mk1 f <> mk2 g
    
instance 
    forall field u s l r a
     . ( KnownSymbol field
       , ToValue a
       ) 
    => GToObject 
         (G.M1 
           G.S
           (G.MetaSel 
             (Just field) 
             u s l) 
            (G.K1 r a)) 
  where
    gToObject = do
      mk <- toValue
      let field = Text.pack (symbolVal @field (Proxy :: Proxy field))
      pure \(G.M1 (G.K1 a)) -> HashMap.singleton field (mk a)
  
class GFromObject f where
  gFromObject :: MonadSupply m => m (HashMap Text Value -> f x)
  
instance GFromObject f => GFromObject (G.M1 G.D t f) where
  gFromObject = (G.M1 .) <$> gFromObject
  
instance GFromObject f => GFromObject (G.M1 G.C t f) where
  gFromObject = (G.M1 .) <$> gFromObject

instance 
    forall field u s l r a
     . ( KnownSymbol field
       , FromValue a
       ) 
    => GFromObject 
         (G.M1 
           G.S
           (G.MetaSel 
             (Just field) 
             u s l) 
            (G.K1 r a)) 
  where
    gFromObject = do
      mk <- fromValue
      let field = Text.pack (symbolVal @field (Proxy :: Proxy field))
      pure \o ->
        case HashMap.lookup field o of
          Nothing -> throw (FieldNotFound field)
          Just v -> G.M1 (G.K1 (mk v))
  
instance (GFromObject f, GFromObject g) => GFromObject (f G.:*: g) where
  gFromObject = do
    mk1 <- gFromObject
    mk2 <- gFromObject
    pure \o -> mk1 o G.:*: mk2 o

builtIn :: ToValue a => Text -> a -> Interpreter.Env
builtIn name value = evalSupply 0 do
  mk <- Interpreter.toValue
  let qualName = Names.mkQualified (Names.Ident name) (Names.ModuleName "Main")
  pure $ Map.singleton qualName $ mk value
   
interpret :: FromValue a => Env -> CoreFn.Module ann -> a
interpret initialEnv CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleDecls } =
  let env = bind moduleName (Just moduleName) initialEnv (fmap void moduleDecls)
      mainFn = eval moduleName env (CoreFn.Var () (Qualified (Just moduleName) (Ident "main")))
   in evalSupply 0 fromValue mainFn

eval :: Names.ModuleName -> Env -> CoreFn.Expr () -> Value
eval mn env (CoreFn.Literal _ lit) =
  evalLit mn env lit
eval mn env (CoreFn.Accessor _ pss e) =
  let val = eval mn env e
      field = evalPSString pss
   in case val of
        Object o ->
          case HashMap.lookup field o of
            Just x -> x
            Nothing -> throw (FieldNotFound field)
eval mn env (CoreFn.Abs _ arg body) =
  Closure (MkClosure env arg (\env' -> eval mn env' body))
eval mn env (CoreFn.App _ f x) =
  apply (eval mn env f) (eval mn env x)
eval mn env (CoreFn.Var _ name) =
  case Map.lookup name env of
    Nothing -> throw $ UnknownIdent name
    Just val -> val
eval mn env (CoreFn.Let _ binders body) = do
  let env' = bind mn Nothing env binders
  eval mn env' body
eval mn env (CoreFn.ObjectUpdate _ e updates) = do
  let val = eval mn env e
      updateOne 
        :: HashMap Text Value
        -> (PSString.PSString, CoreFn.Expr ())
        -> HashMap Text Value
      updateOne o (pss, new) = do
        let field = evalPSString pss
            newVal = eval mn env new
        HashMap.insert field newVal o
  case val of
    Object o -> Object (foldl updateOne o updates)
    _ -> throw (TypeMismatch "object")
eval mn env (CoreFn.Case _ args alts) = do
  let vals = fmap (eval mn env) args
      result = asum (map (match vals) alts)
  case result of
    Nothing -> throw InexhaustivePatternMatch
    Just (newEnv, matchedExpr) -> eval mn (newEnv <> env) matchedExpr
eval mn env (CoreFn.Constructor _ _tyName ctor fields) = 
  Constructor (MkConstructor (Qualified (Just mn) ctor) [] fields)

match :: [Value]
      -> CoreFn.CaseAlternative ()
      -> Maybe (Env, CoreFn.Expr ())
match vals (CoreFn.CaseAlternative binders expr) 
  | length vals == length binders = do
    newEnv <- fold <$> zipWithM matchOne vals binders
    case expr of
      Left guards -> throw (NotSupported "guards")
      Right e -> pure (newEnv, e)
  | otherwise = throw (InvalidNumberOfArguments (length vals) (length binders))

matchOne :: Value -> CoreFn.Binder () -> Maybe Env
matchOne _ (CoreFn.NullBinder _) = pure mempty
matchOne val (CoreFn.LiteralBinder _ lit) = matchLit val lit
matchOne val (CoreFn.VarBinder _ ident) = do
  pure (Map.singleton (Qualified Nothing ident) val)
matchOne val (CoreFn.NamedBinder _ ident b) = do
  env <- matchOne val b
  pure (Map.insert (Qualified Nothing ident) val env)
matchOne (Constructor (MkConstructor ctor vals [])) (CoreFn.ConstructorBinder _ _tyName ctor' bs) 
  | ctor == ctor'
  = fold <$> zipWithM matchOne vals bs
matchOne Constructor{} CoreFn.ConstructorBinder{} =
  throw UnsaturatedConstructorApplication
matchOne _ _ = Nothing

matchLit
  :: Value
  -> CoreFn.Literal (CoreFn.Binder ())
  -> Maybe Env
matchLit (Number n) (CoreFn.NumericLiteral (Left i)) 
  | fromIntegral i == n = pure mempty
matchLit (Number n) (CoreFn.NumericLiteral (Right d))
  | realToFrac d == n = pure mempty
matchLit (String s) (CoreFn.StringLiteral pss) = do
  guard (s == evalPSString pss)
  pure mempty
matchLit (String s) (CoreFn.CharLiteral chr)
  | s == Text.singleton chr = pure mempty
matchLit (Bool b) (CoreFn.BooleanLiteral b')
  | b == b' = pure mempty
matchLit (Array xs) (CoreFn.ArrayLiteral bs)
  | length xs == length bs
  = fold <$> zipWithM matchOne (Vector.toList xs) bs
matchLit (Object o) (CoreFn.ObjectLiteral bs) = do
  let vals = HashMap.fromList [ (t, (t, b)) | (pss, b) <- bs, let t = evalPSString pss ]
  let matchField :: These Value (Text, CoreFn.Binder ()) -> Maybe Env
      matchField This{} = pure mempty
      matchField (That (pss, _)) = throw (FieldNotFound pss)
      matchField (These val (_, b)) = matchOne val b
  fold <$> sequence (Align.alignWith matchField o vals)
matchLit _ _ = Nothing

evalPSString :: PSString.PSString -> Text
evalPSString pss = 
  case PSString.decodeString pss of
    Just field -> field
    _ -> throw (InvalidFieldName pss)

evalLit :: Names.ModuleName -> Env -> CoreFn.Literal (CoreFn.Expr ()) -> Value
evalLit mn env (CoreFn.NumericLiteral (Left int)) =
  Number (fromIntegral int)
evalLit mn env (CoreFn.NumericLiteral (Right dbl)) =
  Number (realToFrac dbl)
evalLit mn env (CoreFn.StringLiteral str) =
  String (evalPSString str)
evalLit mn env (CoreFn.CharLiteral chr) =
  String (Text.singleton chr)
evalLit mn env (CoreFn.BooleanLiteral b) =
  Bool b
evalLit mn env (CoreFn.ArrayLiteral xs) = do
  let vs = fmap (eval mn env) xs
  Array (Vector.fromList vs)
evalLit mn env (CoreFn.ObjectLiteral xs) = do
  let evalField (pss, e) = do
        let field = evalPSString pss
            val = eval mn env e
        (field, val)
      vs = fmap evalField xs
  Object (HashMap.fromList vs)

bind :: Names.ModuleName -> Maybe Names.ModuleName -> Env -> [CoreFn.Bind ()] -> Env
bind mn scope = foldl go where
  go :: Env -> CoreFn.Bind () -> Env
  go env (CoreFn.NonRec _ name e) = 
    Map.insert (Qualified scope name) (eval mn env e) env
  go env (CoreFn.Rec es) = 
    throw (NotSupported "recursive bindings")

apply :: Value -> Value -> Value
apply (Closure MkClosure{ closEnv, closArg, closBody }) arg =
  closBody (Map.insert (Qualified Nothing closArg) arg closEnv)
apply (Constructor MkConstructor{ ctorName, ctorApplied, ctorUnapplied = hd : tl }) arg = do
  Constructor MkConstructor{ ctorName, ctorApplied = arg : ctorApplied, ctorUnapplied = tl }
apply _ _ = throw (TypeMismatch "closure")
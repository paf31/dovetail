{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.PureScript.Interpreter
  ( Env
  , Value(..)
  , Constructor(..)
  , EvalException(..)
  , eval
  , apply
  , interpret
  , ToValue(..)
  , FromValue(..)
  , builtIn
  ) where

import Control.Arrow (Kleisli(..))
import Control.Exception (Exception, throw)
import Control.Monad (guard, foldM, join, mzero, zipWithM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Align qualified as Align
import Data.Foldable (asum, fold)
import Data.Functor (void)
import Data.Functor.Identity (Identity(..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map (Map)
import Data.Map qualified as Map
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

type Env m = Map (Qualified Ident) (Value m)

data Value m
  = Object (HashMap Text (Value m))
  | Array (Vector (Value m))
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  | Closure (Value m -> m (Value m))
  | Constructor (Constructor m)
  
data Constructor m = MkConstructor
  { ctorName :: Qualified (Names.ProperName 'Names.ConstructorName)
  , ctorApplied :: [Value m]
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
  
class Monad m => ToValue m a where
  toValue :: a -> Value m
  
  default toValue :: (G.Generic a, GToObject m (G.Rep a)) => a -> Value m
  toValue = Object . gToObject . G.from
  
instance Monad m => ToValue m (Value m) where
  toValue = id
  
instance Monad m => ToValue m Integer where
  toValue = Number . fromIntegral
  
instance Monad m => ToValue m Scientific where
  toValue = Number
  
instance Monad m => ToValue m Text where
  toValue = String
  
instance Monad m => ToValue m Bool where
  toValue = Bool
  
instance (m ~ n, Monad m, FromValue m a, ToValue m b) => ToValue m (Kleisli n a b) where
  toValue f = Closure (fmap toValue . runKleisli f . fromValue)
  
instance (Monad m, FromValue m a, ToValue m b) => ToValue m (a -> b) where
  toValue f = Closure (pure . toValue . f . fromValue)

instance ToValue m a => ToValue m (Maybe a) where
  toValue = maybe Null toValue

instance ToValue m a => ToValue m (Vector a) where
  toValue = Array . fmap toValue
  
class Monad m => FromValue m a where
  fromValue :: Value m -> a
  
  default fromValue :: (G.Generic a, GFromObject m (G.Rep a)) => Value m -> a
  fromValue = \case
    Object o -> G.to (gFromObject o)
    _ -> throw (TypeMismatch "object")
  
instance Monad m => FromValue m (Value m) where
  fromValue = id
  
instance Monad m => FromValue m Text where
  fromValue = \case
    String s -> s
    _ -> throw (TypeMismatch "string")
  
instance Monad m => FromValue m Integer where
  fromValue = \case
    Number s
      | Right i <- floatingOrInteger @Double s -> i
    _ -> throw (TypeMismatch "integer")
  
instance Monad m => FromValue m Scientific where
  fromValue = \case
    Number s -> s
    _ -> throw (TypeMismatch "number")
  
instance Monad m => FromValue m Bool where
  fromValue = \case
    Bool b -> b
    _ -> throw (TypeMismatch "boolean")
  
instance FromValue m a => FromValue m (Maybe a) where
  fromValue = \case
    Null -> Nothing
    v -> Just (fromValue v)
  
instance FromValue m a => FromValue m (Vector a) where
  fromValue = \case
    Array xs -> fmap fromValue xs
    _ -> throw (TypeMismatch "array")
  
instance (m ~ n, ToValue m a, FromValue m b) => FromValue m (Kleisli n a b) where
  fromValue f = Kleisli \a -> fromValue <$> apply f (toValue a)
  
instance (m ~ Identity, ToValue m a, FromValue m b) => FromValue m (a -> b) where
  fromValue f = fromValue . runIdentity . apply f . toValue
       
class GToObject m f where
  gToObject :: f x -> HashMap Text (Value m)
  
instance GToObject m f => GToObject m (G.M1 G.D t f) where
  gToObject = gToObject . G.unM1
  
instance GToObject m f => GToObject m (G.M1 G.C t f) where
  gToObject = gToObject . G.unM1
  
instance (GToObject m f, GToObject m g) => GToObject m (f G.:*: g) where
  gToObject (f G.:*: g) = gToObject f <> gToObject g
    
instance 
    forall m field u s l r a
     . ( KnownSymbol field
       , ToValue m a
       ) 
    => GToObject m 
         (G.M1 
           G.S
           ('G.MetaSel 
             ('Just field) 
             u s l) 
            (G.K1 r a)) 
  where
    gToObject (G.M1 (G.K1 a)) = do
      let field = Text.pack (symbolVal @field (Proxy :: Proxy field))
       in HashMap.singleton field (toValue a)
  
class GFromObject m f where
  gFromObject :: HashMap Text (Value m) -> f x
  
instance GFromObject m f => GFromObject m (G.M1 G.D t f) where
  gFromObject = G.M1 . gFromObject
  
instance GFromObject m f => GFromObject m (G.M1 G.C t f) where
  gFromObject = G.M1 . gFromObject

instance 
    forall m field u s l r a
     . ( KnownSymbol field
       , FromValue m a
       ) 
    => GFromObject m 
         (G.M1 
           G.S
           ('G.MetaSel 
             ('Just field) 
             u s l) 
            (G.K1 r a)) 
  where
    gFromObject o = do
      let field = Text.pack (symbolVal @field (Proxy :: Proxy field))
      case HashMap.lookup field o of
        Nothing -> throw (FieldNotFound field)
        Just v -> G.M1 . G.K1 $ fromValue v
  
instance (GFromObject m f, GFromObject m g) => GFromObject m (f G.:*: g) where
  gFromObject o = gFromObject o G.:*: gFromObject o

builtIn :: ToValue m a => Text -> a -> Env m
builtIn name value =
  let qualName = Names.mkQualified (Names.Ident name) (Names.ModuleName "Main")
   in Map.singleton qualName $ toValue value
   
interpret :: FromValue m a => Env m -> CoreFn.Module ann -> m a
interpret initialEnv CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleDecls } = do
  env <- bind moduleName (Just moduleName) initialEnv (fmap void moduleDecls)
  mainFn <- eval moduleName env (CoreFn.Var () (Qualified (Just moduleName) (Ident "main")))
  pure (fromValue mainFn)

eval 
  :: forall m
   . Monad m
  => Names.ModuleName
  -> Env m
  -> CoreFn.Expr ()
  -> m (Value m)
eval mn env (CoreFn.Literal _ lit) =
  evalLit mn env lit
eval mn env (CoreFn.Accessor _ pss e) = do
  val <- eval mn env e
  let field = evalPSString pss
  case val of
    Object o ->
      case HashMap.lookup field o of
        Just x -> pure x
        Nothing -> throw (FieldNotFound field)
    _ -> throw (TypeMismatch "object")
eval mn env (CoreFn.Abs _ arg body) =
  pure . Closure $ \v -> eval mn (Map.insert (Qualified Nothing arg) v env) body
eval mn env (CoreFn.App _ f x) =
  join (apply <$> eval mn env f <*> eval mn env x)
eval _ env (CoreFn.Var _ name) =
  case Map.lookup name env of
    Nothing -> throw $ UnknownIdent name
    Just val -> pure val
eval mn env (CoreFn.Let _ binders body) = do
  env' <- bind mn Nothing env binders
  eval mn env' body
eval mn env (CoreFn.ObjectUpdate _ e updates) = do
  val <- eval mn env e
  let updateOne 
        :: HashMap Text (Value m)
        -> (PSString.PSString, CoreFn.Expr ())
        -> m (HashMap Text (Value m))
      updateOne o (pss, new) = do
        let field = evalPSString pss
        newVal <- eval mn env new
        pure $ HashMap.insert field newVal o
  case val of
    Object o -> Object <$> foldM updateOne o updates
    _ -> throw (TypeMismatch "object")
eval mn env (CoreFn.Case _ args alts) = do
  vals <- traverse (eval mn env) args
  result <- runMaybeT (asum (map (match vals) alts))
  case result of
    Nothing -> throw InexhaustivePatternMatch
    Just (newEnv, matchedExpr) -> eval mn (newEnv <> env) matchedExpr
eval mn _ (CoreFn.Constructor _ _tyName ctor fields) = 
  pure . Constructor $ MkConstructor (Qualified (Just mn) ctor) [] fields

match :: Monad m
      => [Value m]
      -> CoreFn.CaseAlternative ()
      -> MaybeT m (Env m, CoreFn.Expr ())
match vals (CoreFn.CaseAlternative binders expr) 
  | length vals == length binders = do
    newEnv <- fold <$> zipWithM matchOne vals binders
    case expr of
      Left _guards -> throw (NotSupported "guards")
      Right e -> pure (newEnv, e)
  | otherwise = throw (InvalidNumberOfArguments (length vals) (length binders))

matchOne :: Monad m => Value m -> CoreFn.Binder () -> MaybeT m (Env m)
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
matchOne _ _ = mzero

matchLit
  :: forall m
   . Monad m
  => Value m
  -> CoreFn.Literal (CoreFn.Binder ())
  -> MaybeT m (Env m)
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
  let matchField :: These (Value m) (Text, CoreFn.Binder ()) -> MaybeT m (Env m)
      matchField This{} = pure mempty
      matchField (That (pss, _)) = throw (FieldNotFound pss)
      matchField (These val (_, b)) = matchOne val b
  fold <$> sequence (Align.alignWith matchField o vals)
matchLit _ _ = mzero

evalPSString :: PSString.PSString -> Text
evalPSString pss = 
  case PSString.decodeString pss of
    Just field -> field
    _ -> throw (InvalidFieldName pss)

evalLit :: Monad m => Names.ModuleName -> Env m -> CoreFn.Literal (CoreFn.Expr ()) -> m (Value m)
evalLit _ _ (CoreFn.NumericLiteral (Left int)) =
  pure $ Number (fromIntegral int)
evalLit _ _ (CoreFn.NumericLiteral (Right dbl)) =
  pure $ Number (realToFrac dbl)
evalLit _ _ (CoreFn.StringLiteral str) =
  pure $ String (evalPSString str)
evalLit _ _ (CoreFn.CharLiteral chr) =
  pure $ String (Text.singleton chr)
evalLit _ _ (CoreFn.BooleanLiteral b) =
  pure $ Bool b
evalLit mn env (CoreFn.ArrayLiteral xs) = do
  vs <- traverse (eval mn env) xs
  pure $ Array (Vector.fromList vs)
evalLit mn env (CoreFn.ObjectLiteral xs) = do
  let evalField (pss, e) = do
        let field = evalPSString pss
        val <- eval mn env e
        pure (field, val)
  Object . HashMap.fromList <$> traverse evalField xs

bind :: forall m. Monad m => Names.ModuleName -> Maybe Names.ModuleName -> Env m -> [CoreFn.Bind ()] -> m (Env m)
bind mn scope = foldM go where
  go :: Env m -> CoreFn.Bind () -> m (Env m)
  go env (CoreFn.NonRec _ name e) = do
    val <- eval mn env e
    pure $ Map.insert (Qualified scope name) val env
  go _ (CoreFn.Rec _) = 
    throw (NotSupported "recursive bindings")

apply :: Monad m => Value m -> Value m -> m (Value m)
apply (Closure f) arg = f arg
apply (Constructor MkConstructor{ ctorName, ctorApplied, ctorUnapplied = _ : tl }) arg = do
  pure $ Constructor MkConstructor{ ctorName, ctorApplied = arg : ctorApplied, ctorUnapplied = tl }
apply _ _ = throw (TypeMismatch "closure")
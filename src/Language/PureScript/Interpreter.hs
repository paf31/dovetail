{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Language.PureScript.Interpreter
  ( Env
  , Value(..)
  , Constructor(..)
  , EvaluationError(..)
  , renderEvaluationError
  , EvalT(..)
  , Eval
  , runEval
  , eval
  , apply
  , interpret
  , ToValue(..)
  , FromValue(..)
  , builtIn
  ) where
 
import Control.Monad (guard, foldM, join, mzero, zipWithM)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
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

newtype EvalT m a = EvalT { runEvalT :: ExceptT EvaluationError m a }
  deriving newtype (Functor, Applicative, Monad, MonadError EvaluationError)

type Eval = EvalT Identity

runEval :: Eval a -> Either EvaluationError a
runEval = runIdentity . runExceptT . runEvalT

data Value m
  = Object (HashMap Text (Value m))
  | Array (Vector (Value m))
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  | Closure (Value m -> EvalT m (Value m))
  | Constructor (Constructor m)
  
data Constructor m = MkConstructor
  { ctorName :: Qualified (Names.ProperName 'Names.ConstructorName)
  , ctorApplied :: [Value m]
  , ctorUnapplied :: [Ident]
  }
  
data EvaluationError 
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

renderEvaluationError :: EvaluationError -> String
renderEvaluationError (UnknownIdent x) =
  "Identifier not in scope: " <> Text.unpack (Names.showQualified Names.showIdent x)
renderEvaluationError (TypeMismatch x) =
  "Type mismatch, expected " <> Text.unpack x
renderEvaluationError (FieldNotFound x) =
  "Record field not found: " <> Text.unpack x
renderEvaluationError InexhaustivePatternMatch =
  "Inexhaustive pattern match"
renderEvaluationError (NotSupported x) =
  "Unsupported feature: " <> Text.unpack x
renderEvaluationError (InvalidNumberOfArguments given expected) =
  "Invalid number of arguments, given " <> show given <> ", but expected " <> show expected
renderEvaluationError UnsaturatedConstructorApplication =
  "Unsaturated constructor application"
renderEvaluationError (InvalidFieldName x) =
  "Invalid field name: " <> PSString.decodeStringWithReplacement x
renderEvaluationError (OtherError x) =
  "Other error: " <> Text.unpack x
  
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

class ToValueRHS m a where
  toValueRHS :: a -> EvalT m (Value m)
  
instance (Monad m, FromValue m a, ToValueRHS m b) => ToValueRHS m (a -> b) where
  toValueRHS f = pure (Closure (\v -> toValueRHS . f =<< fromValue v))
   
instance (ToValue m a, n ~ m) => ToValueRHS m (EvalT n a) where
  toValueRHS = fmap toValue

instance (Monad m, FromValue m a, ToValueRHS m b) => ToValue m (a -> b) where
  toValue f = Closure (\v -> toValueRHS . f =<< fromValue v)

instance ToValue m a => ToValue m (Maybe a) where
  toValue = maybe Null toValue

instance ToValue m a => ToValue m (Vector a) where
  toValue = Array . fmap toValue
  
class Monad m => FromValue m a where
  fromValue :: Value m -> EvalT m a
  
  default fromValue :: (G.Generic a, GFromObject m (G.Rep a)) => Value m -> EvalT m a
  fromValue = \case
    Object o -> G.to <$> gFromObject o
    _ -> throwError (TypeMismatch "object")
  
instance Monad m => FromValue m (Value m) where
  fromValue = pure
  
instance Monad m => FromValue m Text where
  fromValue = \case
    String s -> pure s
    _ -> throwError (TypeMismatch "string")
  
instance Monad m => FromValue m Integer where
  fromValue = \case
    Number s
      | Right i <- floatingOrInteger @Double s -> pure i
    _ -> throwError (TypeMismatch "integer")
  
instance Monad m => FromValue m Scientific where
  fromValue = \case
    Number s -> pure s
    _ -> throwError (TypeMismatch "number")
  
instance Monad m => FromValue m Bool where
  fromValue = \case
    Bool b -> pure b
    _ -> throwError (TypeMismatch "boolean")
  
instance FromValue m a => FromValue m (Maybe a) where
  fromValue = \case
    Null -> pure Nothing
    v -> Just <$> fromValue v
  
instance FromValue m a => FromValue m (Vector a) where
  fromValue = \case
    Array xs -> traverse fromValue xs
    _ -> throwError (TypeMismatch "array")
  
class FromValueRHS m a where
  fromValueRHS :: EvalT m (Value m) -> a
  
instance (Monad m, ToValue m a, FromValueRHS m b) => FromValueRHS m (a -> b) where
  fromValueRHS mv a = fromValueRHS do
    v <- mv
    fromValueRHS (apply v (toValue a))
   
instance (FromValue m a, n ~ m) => FromValueRHS m (EvalT n a) where
  fromValueRHS = (>>= fromValue)
  
instance (Monad m, FromValueRHS m b, ToValue m a) => FromValue m (a -> b) where
  fromValue f = pure $ \a -> fromValueRHS (apply f (toValue a))
       
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
  gFromObject :: HashMap Text (Value m) -> EvalT m (f x)
  
instance (Functor m, GFromObject m f) => GFromObject m (G.M1 G.D t f) where
  gFromObject = fmap G.M1 . gFromObject
  
instance (Functor m, GFromObject m f) => GFromObject m (G.M1 G.C t f) where
  gFromObject = fmap G.M1 . gFromObject

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
        Nothing -> throwError (FieldNotFound field)
        Just v -> G.M1 . G.K1 <$> fromValue v
  
instance (Monad m, GFromObject m f, GFromObject m g) => GFromObject m (f G.:*: g) where
  gFromObject o = (G.:*:) <$> gFromObject o <*> gFromObject o

builtIn :: ToValue m a => Text -> a -> Env m
builtIn name value =
  let qualName = Names.mkQualified (Names.Ident name) (Names.ModuleName "Main")
   in Map.singleton qualName $ toValue value
   
interpret :: FromValue m a => Env m -> CoreFn.Module ann -> EvalT m a
interpret initialEnv CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleDecls } = do
  env <- bind moduleName (Just moduleName) initialEnv (fmap void moduleDecls)
  mainFn <- eval moduleName env (CoreFn.Var () (Qualified (Just moduleName) (Ident "main")))
  fromValue mainFn

eval 
  :: forall m
   . Monad m
  => Names.ModuleName
  -> Env m
  -> CoreFn.Expr ()
  -> EvalT m (Value m)
eval mn env (CoreFn.Literal _ lit) =
  evalLit mn env lit
eval mn env (CoreFn.Accessor _ pss e) = do
  val <- eval mn env e
  field <- evalPSString pss
  case val of
    Object o ->
      case HashMap.lookup field o of
        Just x -> pure x
        Nothing -> throwError (FieldNotFound field)
    _ -> throwError (TypeMismatch "object")
eval mn env (CoreFn.Abs _ arg body) =
  pure . Closure $ \v -> eval mn (Map.insert (Qualified Nothing arg) v env) body
eval mn env (CoreFn.App _ f x) =
  join (apply <$> eval mn env f <*> eval mn env x)
eval _ env (CoreFn.Var _ name) =
  case Map.lookup name env of
    Nothing -> throwError $ UnknownIdent name
    Just val -> pure val
eval mn env (CoreFn.Let _ binders body) = do
  env' <- bind mn Nothing env binders
  eval mn env' body
eval mn env (CoreFn.ObjectUpdate _ e updates) = do
  val <- eval mn env e
  let updateOne 
        :: HashMap Text (Value m)
        -> (PSString.PSString, CoreFn.Expr ())
        -> EvalT m (HashMap Text (Value m))
      updateOne o (pss, new) = do
        field <- evalPSString pss
        newVal <- eval mn env new
        pure $ HashMap.insert field newVal o
  case val of
    Object o -> Object <$> foldM updateOne o updates
    _ -> throwError (TypeMismatch "object")
eval mn env (CoreFn.Case _ args alts) = do
  vals <- traverse (eval mn env) args
  result <- runMaybeT (asum (map (match vals) alts))
  case result of
    Nothing -> throwError InexhaustivePatternMatch
    Just (newEnv, matchedExpr) -> eval mn (newEnv <> env) matchedExpr
eval mn _ (CoreFn.Constructor _ _tyName ctor fields) = 
  pure . Constructor $ MkConstructor (Qualified (Just mn) ctor) [] fields

match :: Monad m
      => [Value m]
      -> CoreFn.CaseAlternative ()
      -> MaybeT (EvalT m) (Env m, CoreFn.Expr ())
match vals (CoreFn.CaseAlternative binders expr) 
  | length vals == length binders = do
    newEnv <- fold <$> zipWithM matchOne vals binders
    case expr of
      Left _guards -> throwError (NotSupported "guards")
      Right e -> pure (newEnv, e)
  | otherwise = throwError (InvalidNumberOfArguments (length vals) (length binders))

matchOne 
  :: Monad m
  => Value m
  -> CoreFn.Binder ()
  -> MaybeT (EvalT m) (Env m)
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
  throwError UnsaturatedConstructorApplication
matchOne _ _ = mzero

matchLit
  :: forall m
   . Monad m
  => Value m
  -> CoreFn.Literal (CoreFn.Binder ())
  -> MaybeT (EvalT m) (Env m)
matchLit (Number n) (CoreFn.NumericLiteral (Left i)) 
  | fromIntegral i == n = pure mempty
matchLit (Number n) (CoreFn.NumericLiteral (Right d))
  | realToFrac d == n = pure mempty
matchLit (String s) (CoreFn.StringLiteral pss) = do
  s' <- lift (evalPSString pss)
  guard (s == s')
  pure mempty
matchLit (String s) (CoreFn.CharLiteral chr)
  | s == Text.singleton chr = pure mempty
matchLit (Bool b) (CoreFn.BooleanLiteral b')
  | b == b' = pure mempty
matchLit (Array xs) (CoreFn.ArrayLiteral bs)
  | length xs == length bs
  = fold <$> zipWithM matchOne (Vector.toList xs) bs
matchLit (Object o) (CoreFn.ObjectLiteral bs) = do
  let evalField (pss, b) = do
        t <- lift (evalPSString pss)
        pure (t, (t, b))
  vals <- HashMap.fromList <$> traverse evalField bs
  let matchField :: These (Value m) (Text, CoreFn.Binder ()) -> MaybeT (EvalT m) (Env m)
      matchField This{} = pure mempty
      matchField (That (pss, _)) = throwError (FieldNotFound pss)
      matchField (These val (_, b)) = matchOne val b
  fold <$> sequence (Align.alignWith matchField o vals)
matchLit _ _ = mzero

evalPSString :: Monad m => PSString.PSString -> EvalT m Text
evalPSString pss = 
  case PSString.decodeString pss of
    Just field -> pure field
    _ -> throwError (InvalidFieldName pss)

evalLit :: Monad m => Names.ModuleName -> Env m -> CoreFn.Literal (CoreFn.Expr ()) -> EvalT m (Value m)
evalLit _ _ (CoreFn.NumericLiteral (Left int)) =
  pure $ Number (fromIntegral int)
evalLit _ _ (CoreFn.NumericLiteral (Right dbl)) =
  pure $ Number (realToFrac dbl)
evalLit _ _ (CoreFn.StringLiteral str) =
  String <$> evalPSString str
evalLit _ _ (CoreFn.CharLiteral chr) =
  pure $ String (Text.singleton chr)
evalLit _ _ (CoreFn.BooleanLiteral b) =
  pure $ Bool b
evalLit mn env (CoreFn.ArrayLiteral xs) = do
  vs <- traverse (eval mn env) xs
  pure $ Array (Vector.fromList vs)
evalLit mn env (CoreFn.ObjectLiteral xs) = do
  let evalField (pss, e) = do
        field <- evalPSString pss
        val <- eval mn env e
        pure (field, val)
  Object . HashMap.fromList <$> traverse evalField xs

bind 
  :: forall m
   . Monad m
  => Names.ModuleName
  -> Maybe Names.ModuleName
  -> Env m
  -> [CoreFn.Bind ()] 
  -> EvalT m (Env m)
bind mn scope = foldM go where
  go :: Env m -> CoreFn.Bind () -> EvalT m (Env m)
  go env (CoreFn.NonRec _ name e) = do
    val <- eval mn env e
    pure $ Map.insert (Qualified scope name) val env
  go _ (CoreFn.Rec _) = 
    throwError (NotSupported "recursive bindings")

apply
  :: Monad m
  => Value m
  -> Value m
  -> EvalT m (Value m)
apply (Closure f) arg = f arg
apply (Constructor MkConstructor{ ctorName, ctorApplied, ctorUnapplied = _ : tl }) arg = do
  pure $ Constructor MkConstructor{ ctorName, ctorApplied = arg : ctorApplied, ctorUnapplied = tl }
apply _ _ = throwError (TypeMismatch "closure")
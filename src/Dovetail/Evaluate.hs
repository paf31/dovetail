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

module Dovetail.Evaluate
  ( 
  -- * High-level API
    buildCoreFn
  , builtIn
  
  -- * Evaluation
  
  -- ** Eval/apply
  , eval
  , apply
  
  -- * Conversion to and from Haskell types
  , ToValue(..)
  -- ** Higher-order functions
  , ToValueRHS(..)
  -- ** Records
  , ObjectOptions(..)
  , defaultObjectOptions
  , genericToValue
  , genericFromValue
  , ToObject(..)
  
  , module Dovetail.Types
  ) where
 
import Control.Monad (guard, foldM, mzero, zipWithM)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Reader.Class
import Data.Align qualified as Align
import Data.Foldable (asum, fold)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.These (These(..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail.Types
import GHC.Generics qualified as G
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Names (Qualified(..))
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString

-- | Evaluate each of the bindings in a compiled PureScript module, and store
-- the evaluated values in the environment, without evaluating any main
-- expression.
buildCoreFn :: MonadFix m => Env m -> CoreFn.Module CoreFn.Ann -> EvalT m (Env m)
buildCoreFn env CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleDecls } = 
  bind (Just moduleName) env moduleDecls
  
-- | Create an environment from a Haskell value.
--
-- It is recommended that a type annotation is given for the type of the value
-- being provided.
--
-- For example:
--
-- @
-- builtIn (ModuleName "Main") "greeting" ("Hello, World!" :: Text)
-- builtIn (ModuleName "Main") "somePrimes" ([2, 3, 5, 7, 11] :: Vector Integer)
-- @
--
-- Functions can be provided as built-ins, but the 'EvalT' monad needs to be
-- used to wrap any outputs (or values in positive position):
--
-- @
-- builtIn (ModuleName "Main") "strip" ((pure . Text.strip) :: Text -> EvalT m Text)
-- builtIn (ModuleName "Main") "map" (traverse :: (Value -> EvalT m Value) -> Vector Value -> EvalT m (Vector Value))
-- @
--
-- Polymorphic functions can also be provided as built-ins, but values with 
-- polymoprhic types will need to be passed across the FFI boundary with 
-- monomorphic types. The type 'Value' can always be used to represent values of
-- unknown or polymorphic type, as in the @map@ example above.
builtIn :: ToValue m a => Names.ModuleName -> Text -> a -> Env m
builtIn mn name value =
  let qualName = Names.mkQualified (Names.Ident name) mn
   in Map.singleton qualName $ toValue value

evalPSString :: MonadFix m => PSString.PSString -> EvalT m Text
evalPSString pss = 
  case PSString.decodeString pss of
    Just field -> pure field
    _ -> throwErrorWithContext (InvalidFieldName pss)

-- | Evaluate a PureScript CoreFn expression in the given environment.
--
-- Note: it should not be necessary to call this function directly in most
-- circumstances. It is provided as a helper function, for some more advanced
-- use cases, such as setting up a custom environment.
eval 
  :: forall m
   . MonadFix m
  => Env m
  -> CoreFn.Expr CoreFn.Ann
  -> EvalT m (Value m)
eval env expr = pushStackFrame env expr (evalHelper expr) where
  evalHelper (CoreFn.Literal _ lit) = 
    evalLit env lit
  evalHelper (CoreFn.Accessor _ pss e) = do
    val <- eval env e
    field <- evalPSString pss
    case val of
      Object o ->
        case HashMap.lookup field o of
          Just x -> pure x
          Nothing -> throwErrorWithContext (FieldNotFound field val)
      _ -> throwErrorWithContext (TypeMismatch "object" val)
  evalHelper (CoreFn.Abs _ arg body) = do
    ctx <- ask
    pure . Closure $ \v -> local (const ctx) $ eval (Map.insert (Qualified Nothing arg) v env) body
  evalHelper (CoreFn.App _ f x) = do
    x_ <- eval env x
    f_ <- eval env f
    apply f_ x_
  evalHelper (CoreFn.Var _ name) =
    case Map.lookup name env of
      Nothing -> throwErrorWithContext $ UnknownIdent name
      Just val -> pure val
  evalHelper (CoreFn.Let _ binders body) = do
    env' <- bind Nothing env binders
    eval env' body
  evalHelper (CoreFn.ObjectUpdate _ e updates) = do
    val <- eval env e
    let updateOne 
          :: HashMap Text (Value m)
          -> (PSString.PSString, CoreFn.Expr CoreFn.Ann)
          -> EvalT m (HashMap Text (Value m))
        updateOne o (pss, new) = do
          field <- evalPSString pss
          newVal <- eval env new
          pure $ HashMap.insert field newVal o
    case val of
      Object o -> Object <$> foldM updateOne o updates
      _ -> throwErrorWithContext (TypeMismatch "object" val)
  evalHelper (CoreFn.Case _ args alts) = do
    vals <- traverse (eval env) args
    result <- runMaybeT (asum (map (match env vals) alts))
    case result of
      Nothing -> throwErrorWithContext (InexhaustivePatternMatch vals)
      Just (newEnv, matchedExpr) -> eval (newEnv <> env) matchedExpr
  evalHelper (CoreFn.Constructor _ _tyName ctor fields) = 
      pure $ go fields []
    where
      go [] applied = Constructor ctor (reverse applied)
      go (_ : tl) applied = Closure \arg -> pure (go tl (arg : applied))

match :: MonadFix m
      => Env m
      -> [Value m]
      -> CoreFn.CaseAlternative CoreFn.Ann
      -> MaybeT (EvalT m) (Env m, CoreFn.Expr CoreFn.Ann)
match env vals (CoreFn.CaseAlternative binders expr) 
  | length vals == length binders = do
    newEnv <- fold <$> zipWithM matchOne vals binders
    case expr of
      Left guards -> (newEnv, ) <$> asum (map (uncurry (evalGuard env)) guards)
      Right e -> pure (newEnv, e)
  | otherwise = throwErrorWithContext (InvalidNumberOfArguments (length vals) (length binders))

evalGuard
  :: MonadFix m
  => Env m
  -> CoreFn.Guard CoreFn.Ann
  -> CoreFn.Expr CoreFn.Ann
  -> MaybeT (EvalT m) (CoreFn.Expr CoreFn.Ann)
evalGuard env g e = do
  test <- lift $ eval env g
  case test of
    Bool b -> guard b
    _ -> throwErrorWithContext (TypeMismatch "boolean" test )
  pure e

matchOne 
  :: MonadFix m
  => Value m
  -> CoreFn.Binder CoreFn.Ann
  -> MaybeT (EvalT m) (Env m)
matchOne _ (CoreFn.NullBinder _) = pure mempty
matchOne val (CoreFn.LiteralBinder _ lit) = matchLit val lit
matchOne val (CoreFn.VarBinder _ ident) = do
  pure (Map.singleton (Qualified Nothing ident) val)
matchOne val (CoreFn.NamedBinder _ ident b) = do
  env <- matchOne val b
  pure (Map.insert (Qualified Nothing ident) val env)
matchOne (Constructor ctor vals) (CoreFn.ConstructorBinder _ _tyName ctor' bs) 
  | ctor == Names.disqualify ctor'
  = if length vals == length bs 
      then fold <$> zipWithM matchOne vals bs
      else throwErrorWithContext UnsaturatedConstructorApplication
matchOne _ _ = mzero

matchLit
  :: forall m
   . MonadFix m
  => Value m
  -> CoreFn.Literal (CoreFn.Binder CoreFn.Ann)
  -> MaybeT (EvalT m) (Env m)
matchLit (Int n) (CoreFn.NumericLiteral (Left i)) 
  | fromIntegral i == n = pure mempty
matchLit (Number n) (CoreFn.NumericLiteral (Right d))
  | realToFrac d == n = pure mempty
matchLit (String s) (CoreFn.StringLiteral pss) = do
  s' <- lift (evalPSString pss)
  guard (s == s')
  pure mempty
matchLit (Char c) (CoreFn.CharLiteral c')
  | c == c' = pure mempty
matchLit (Bool b) (CoreFn.BooleanLiteral b')
  | b == b' = pure mempty
matchLit (Array xs) (CoreFn.ArrayLiteral bs)
  | length xs == length bs
  = fold <$> zipWithM matchOne (Vector.toList xs) bs
matchLit val@(Object o) (CoreFn.ObjectLiteral bs) = do
  let evalField (pss, b) = do
        t <- lift (evalPSString pss)
        pure (t, (t, b))
  vals <- HashMap.fromList <$> traverse evalField bs
  let matchField :: These (Value m) (Text, CoreFn.Binder CoreFn.Ann) -> MaybeT (EvalT m) (Env m)
      matchField This{} = pure mempty
      matchField (That (pss, _)) = throwErrorWithContext (FieldNotFound pss val)
      matchField (These val' (_, b)) = matchOne val' b
  fold <$> sequence (Align.alignWith matchField o vals)
matchLit _ _ = mzero

evalLit :: MonadFix m => Env m -> CoreFn.Literal (CoreFn.Expr CoreFn.Ann) -> EvalT m (Value m)
evalLit _ (CoreFn.NumericLiteral (Left int)) =
  pure $ Int (fromIntegral int)
evalLit _ (CoreFn.NumericLiteral (Right dbl)) =
  pure $ Number (realToFrac dbl)
evalLit _ (CoreFn.StringLiteral str) =
  String <$> evalPSString str
evalLit _ (CoreFn.CharLiteral chr) =
  pure $ Char chr
evalLit _ (CoreFn.BooleanLiteral b) =
  pure $ Bool b
evalLit env (CoreFn.ArrayLiteral xs) = do
  vs <- traverse (eval env) xs
  pure $ Array (Vector.fromList vs)
evalLit env (CoreFn.ObjectLiteral xs) = do
  let evalField (pss, e) = do
        field <- evalPSString pss
        val <- eval env e
        pure (field, val)
  Object . HashMap.fromList <$> traverse evalField xs

bind 
  :: forall m
   . MonadFix m
  => Maybe Names.ModuleName
  -> Env m
  -> [CoreFn.Bind CoreFn.Ann] 
  -> EvalT m (Env m)
bind scope = foldM go where
  go :: Env m -> CoreFn.Bind CoreFn.Ann -> EvalT m (Env m)
  go env (CoreFn.NonRec _ name e) = do
    val <- eval env e
    pure $ Map.insert (Qualified scope name) val env
  go env (CoreFn.Rec exprs) = mfix \newEnv -> do
    vals <- flip traverse exprs \((_, name), e) -> do
      val <- eval newEnv e
      pure $ Map.singleton (Qualified scope name) val
    pure (fold vals <> env)

-- | Apply a value which represents an unevaluated closure to an argument.
apply
  :: MonadFix m
  => Value m
  -> Value m
  -> EvalT m (Value m)
apply (Closure f) arg = f arg
apply val _ = throwErrorWithContext (TypeMismatch "closure" val)

-- | Values which can be communicated across the FFI boundary from Haskell to 
-- PureScript.
--
-- Instances should identify and document any valid representations as a subset 
-- of the semantic domain 'Value'. Such a subset can be identified by an
-- injective function 'toValue', and a partial inverse, 'fromValue', defined
-- on the image of 'toValue'.
--
-- Laws:
--
-- @
-- fromValue . toValue = pure
-- @
class MonadFix m => ToValue m a where
  toValue :: a -> Value m
  
  -- | The default implementation uses generic deriving to identify a Haskell
  -- record type with a single data constructor with a PureScript record with
  -- the same field names.
  default toValue :: (G.Generic a, ToObject m (G.Rep a)) => a -> Value m
  toValue = genericToValue defaultObjectOptions
  
  fromValue :: Value m -> EvalT m a
  
  default fromValue :: (G.Generic a, ToObject m (G.Rep a)) => Value m -> EvalT m a
  fromValue = genericFromValue defaultObjectOptions
  
instance MonadFix m => ToValue m (Value m) where
  toValue = id
  fromValue = pure

-- | The Haskell 'Integer' type corresponds to PureScript's integer type.
instance MonadFix m => ToValue m Integer where
  toValue = Int
  fromValue = \case
    Int i -> pure i
    val -> throwErrorWithContext (TypeMismatch "integer" val)
  
-- | The Haskell 'Douvle' type corresponds to the subset of PureScript
-- values consisting of its Number type.
instance MonadFix m => ToValue m Double where
  toValue = Number
  fromValue = \case
    Number s -> pure s
    val -> throwErrorWithContext (TypeMismatch "number" val)

-- | The Haskell 'Text' type is represented by PureScript strings
-- which contain no lone surrogates.
instance MonadFix m => ToValue m Text where
  toValue = String
  fromValue = \case
    String s -> pure s
    val -> throwErrorWithContext (TypeMismatch "string" val)

-- | The Haskell 'Char' type is represented by PureScript characters.
instance MonadFix m => ToValue m Char where
  toValue = Char
  fromValue = \case
    Char c -> pure c
    val -> throwErrorWithContext (TypeMismatch "char" val)

-- | Haskell booleans are represented by boolean values.
instance MonadFix m => ToValue m Bool where
  toValue = Bool
  fromValue = \case
    Bool b -> pure b
    val -> throwErrorWithContext (TypeMismatch "boolean" val)
  
-- | Haskell functions are represented as closures which take valid
-- representations for the domain type to valid representations of the codomain
-- type.
instance (MonadFix m, ToValue m a, ToValueRHS m b) => ToValue m (a -> b) where
  toValue f = Closure (\v -> toValueRHS . f =<< fromValue v)
  fromValue f = pure $ \a -> fromValueRHS (apply f (toValue a))

-- | Haskell vectors are represented as homogeneous vectors of values, each of
-- which are valid representations of the element type.
instance ToValue m a => ToValue m (Vector a) where
  toValue = Array . fmap toValue
  fromValue = \case
    Array xs -> traverse fromValue xs
    val -> throwErrorWithContext (TypeMismatch "array" val)
    
-- | 'ToValue' should support functions with types such as
--
-- @
-- a -> EvalT m b
-- a -> b -> EvalT m c
-- a -> b -> c -> EvalT m d
-- (a -> EvalT m b) -> EvalT m c
-- (a -> b -> EvalT m c) -> EvalT m d
-- @
--
-- Note that every type in a return position is wrapped in the 'EvalT' monad
-- transformer. This is because evaluation in general may result in errors.
-- However, a naive translation would result in too many applications of 'EvalT'.
--
-- Specifically, we do not want to require types such as these, in which 'EvalT'
-- appears on the right hand side of every function arrow:
--
-- @
-- a -> EvalT m b (b -> EvalT m c)
-- a -> EvalT m b (b -> EvalT m (c -> EvalT m d))
-- @
--
-- For this reason, the 'ToValue' instance for functions delegates to this
-- type class for the type on the right hand side of the function. It skips the
-- application of 'EvalT' for nested function types.
class ToValueRHS m a where
  toValueRHS :: a -> EvalT m (Value m)
  fromValueRHS :: EvalT m (Value m) -> a
  
instance (MonadFix m, ToValue m a, ToValueRHS m b) => ToValueRHS m (a -> b) where
  toValueRHS f = pure (Closure (\v -> toValueRHS . f =<< fromValue v))
  fromValueRHS mv a = fromValueRHS do
    v <- mv
    fromValueRHS (apply v (toValue a))
   
instance (ToValue m a, n ~ m) => ToValueRHS m (EvalT n a) where
  toValueRHS = fmap toValue
  fromValueRHS = (>>= fromValue)
  
-- | Options for customizing generic deriving of record instances
data ObjectOptions = ObjectOptions
  { toPureScriptField :: Text -> Text
  -- ^ Map a Haskell field name to a PureScript field name on the corresponding
  -- record type.
  }
  
-- | * Maps Haskell field names to PureScript field names, unmodified.
defaultObjectOptions :: ObjectOptions
defaultObjectOptions = ObjectOptions
  { toPureScriptField = id
  }

-- | Derived 'toValue' function for Haskell record types which should map to 
-- corresponding PureScript record types.
genericToValue 
  :: (MonadFix m, G.Generic a, ToObject m (G.Rep a))
  => ObjectOptions
  -> a
  -> Value m
genericToValue opts = Object . toObject opts . G.from

-- | Derived 'fromValue' function for Haskell record types which should map to 
-- corresponding PureScript record types.
genericFromValue
  :: (MonadFix m, G.Generic a, ToObject m (G.Rep a))
  => ObjectOptions
  -> Value m
  -> EvalT m a
genericFromValue opts = \case
  Object o -> G.to <$> fromObject opts o
  val -> throwErrorWithContext (TypeMismatch "object" val)
       
-- | This class is used in the default instance for 'ToValue', via generic
-- deriving, in order to identify a Haskell record type (with a single data
-- constructor and named fields) with values in the semantic domain
-- corresponding to a PureScript record type with the same field names.
class ToObject m f where
  toObject :: ObjectOptions -> f x -> HashMap Text (Value m)
  fromObject :: ObjectOptions -> HashMap Text (Value m) -> EvalT m (f x)
  
instance (Functor m, ToObject m f) => ToObject m (G.M1 G.D t f) where
  toObject opts = toObject opts . G.unM1
  fromObject opts = fmap G.M1 . fromObject opts
  
instance (Functor m, ToObject m f) => ToObject m (G.M1 G.C t f) where
  toObject opts = toObject opts . G.unM1
  fromObject opts = fmap G.M1 . fromObject opts
  
instance (MonadFix m, ToObject m f, ToObject m g) => ToObject m (f G.:*: g) where
  toObject opts (f G.:*: g) = toObject opts f <> toObject opts g
  fromObject opts o = (G.:*:) <$> fromObject opts o <*> fromObject opts o
    
instance 
    forall m field u s l r a
     . ( KnownSymbol field
       , ToValue m a
       ) 
    => ToObject m 
         (G.M1 
           G.S
           ('G.MetaSel 
             ('Just field) 
             u s l) 
            (G.K1 r a)) 
  where
    toObject opts (G.M1 (G.K1 a)) = do
      let field = toPureScriptField opts (Text.pack (symbolVal @field (Proxy :: Proxy field)))
       in HashMap.singleton field (toValue a)
    fromObject opts o = do
      let field = toPureScriptField opts (Text.pack (symbolVal @field (Proxy :: Proxy field)))
      case HashMap.lookup field o of
        Nothing -> throwErrorWithContext (FieldNotFound field (Object o))
        Just v -> G.M1 . G.K1 <$> fromValue v
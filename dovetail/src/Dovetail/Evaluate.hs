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
  -- ** Foreign data types
  , ForeignType(..)
  -- ** Records
  , ObjectOptions(..)
  , defaultObjectOptions
  , genericToValue
  , genericFromValue
  , ToObject(..)
  
  , module Dovetail.Types
  
  -- ** Utilities
  , evalPSString
  ) where
 
import Control.Monad (guard, foldM, mzero, zipWithM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Reader.Class
import Data.Align qualified as Align
import Data.Dynamic qualified as Dynamic
import Data.Foldable (asum, fold, for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.These (These(..))
import Data.Typeable (Typeable, TypeRep, typeRep)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail.Evaluate.Internal qualified as Internal
import Dovetail.Types
import GHC.Generics qualified as G
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.PureScript.Constants.Prim qualified as Prim
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Names (Qualified(..))
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString

-- | Evaluate each of the bindings in a compiled PureScript module, and store
-- the evaluated values in the environment, without evaluating any main
-- expression.
buildCoreFn :: Env ctx -> CoreFn.Module CoreFn.Ann -> Eval ctx (Env ctx)
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
-- builtIn (ModuleName "Main") "strip" ((pure . Text.strip) :: Text -> Eval ctx Text)
-- builtIn (ModuleName "Main") "map" (traverse :: (Value -> Eval ctx Value) -> Vector Value -> Eval ctx (Vector Value))
-- @
--
-- Polymorphic functions can also be provided as built-ins, but values with 
-- polymoprhic types will need to be passed across the FFI boundary with 
-- monomorphic types. The type 'Value' can always be used to represent values of
-- unknown or polymorphic type, as in the @map@ example above.
builtIn :: ToValue ctx a => Names.ModuleName -> Text -> a -> Env ctx
builtIn mn name value =
  let qualName = Names.mkQualified (Names.Ident name) mn
   in envFromMap . Map.singleton qualName $ toValue value

evalPSString :: PSString.PSString -> Eval ctx Text
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
  :: forall ctx
   . Env ctx
  -> CoreFn.Expr CoreFn.Ann
  -> Eval ctx (Value ctx)
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
  evalHelper (CoreFn.Abs (_, _, _, Just CoreFn.IsNewtype) _ _) = do
    pure . Closure $ pure
  evalHelper (CoreFn.Abs (_, _, _, Just CoreFn.IsTypeClassConstructor) _ _) = do
    pure . Closure $ pure
  evalHelper (CoreFn.Abs _ arg body) = do
    ctx <- ask
    pure . Closure $ \v -> local (const ctx) $ eval (bindEnv [(Qualified Nothing arg, v)] env) body
  evalHelper (CoreFn.App _ f x) = do
    x_ <- eval env x
    f_ <- eval env f
    apply f_ x_
  evalHelper (CoreFn.Var _ (Qualified (Just Prim.Prim) (Names.Ident u))) | u == Prim.undefined =
    pure (Object mempty)
  evalHelper (CoreFn.Var _ name) =
    case lookupEnv name env of
      Nothing -> throwErrorWithContext $ UnknownIdent name
      Just val -> pure val
  evalHelper (CoreFn.Let _ binders body) = do
    env' <- bind Nothing env binders
    eval env' body
  evalHelper (CoreFn.ObjectUpdate _ e updates) = do
    val <- eval env e
    let updateOne 
          :: HashMap Text (Value ctx)
          -> (PSString.PSString, CoreFn.Expr CoreFn.Ann)
          -> Eval ctx (HashMap Text (Value ctx))
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

match :: Env ctx
      -> [Value ctx]
      -> CoreFn.CaseAlternative CoreFn.Ann
      -> MaybeT (Eval ctx) (Env ctx, CoreFn.Expr CoreFn.Ann)
match env vals (CoreFn.CaseAlternative binders expr) 
  | length vals == length binders = do
    newEnv <- fold <$> zipWithM matchOne vals binders
    case expr of
      Left guards -> (newEnv, ) <$> asum (map (uncurry (evalGuard (newEnv <> env))) guards)
      Right e -> pure (newEnv, e)
  | otherwise = throwErrorWithContext (InvalidNumberOfArguments (length vals) (length binders))

evalGuard
  :: Env ctx
  -> CoreFn.Guard CoreFn.Ann
  -> CoreFn.Expr CoreFn.Ann
  -> MaybeT (Eval ctx) (CoreFn.Expr CoreFn.Ann)
evalGuard env g e = do
  test <- lift $ eval env g
  case test of
    Bool b -> guard b
    _ -> throwErrorWithContext (TypeMismatch "boolean" test )
  pure e

matchOne 
  :: Value ctx
  -> CoreFn.Binder CoreFn.Ann
  -> MaybeT (Eval ctx) (Env ctx)
matchOne _ (CoreFn.NullBinder _) = pure mempty
matchOne val (CoreFn.LiteralBinder _ lit) = matchLit val lit
matchOne val (CoreFn.VarBinder _ ident) = do
  pure (envFromMap (Map.singleton (Qualified Nothing ident) val))
matchOne val (CoreFn.NamedBinder _ ident b) = do
  env <- matchOne val b
  pure (bindEnv [(Qualified Nothing ident, val)] env)
matchOne val (CoreFn.ConstructorBinder (_, _, _, Just meta) _tyName _ctor [b]) 
  | meta == CoreFn.IsNewtype || meta == CoreFn.IsTypeClassConstructor
  = matchOne val b
matchOne (Constructor ctor vals) (CoreFn.ConstructorBinder _ _tyName ctor' bs) 
  | ctor == Names.disqualify ctor'
  = if length vals == length bs 
      then fold <$> zipWithM matchOne vals bs
      else throwErrorWithContext UnsaturatedConstructorApplication
matchOne _ _ = mzero

matchLit
  :: forall ctx
   . Value ctx
  -> CoreFn.Literal (CoreFn.Binder CoreFn.Ann)
  -> MaybeT (Eval ctx) (Env ctx)
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
  let matchField :: These (Value ctx) (Text, CoreFn.Binder CoreFn.Ann) -> MaybeT (Eval ctx) (Env ctx)
      matchField This{} = pure mempty
      matchField (That (pss, _)) = throwErrorWithContext (FieldNotFound pss val)
      matchField (These val' (_, b)) = matchOne val' b
  fold <$> sequence (Align.alignWith matchField o vals)
matchLit _ _ = mzero

evalLit :: Env ctx -> CoreFn.Literal (CoreFn.Expr CoreFn.Ann) -> Eval ctx (Value ctx)
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
  :: Maybe Names.ModuleName
  -> Env ctx
  -> [CoreFn.Bind CoreFn.Ann] 
  -> Eval ctx (Env ctx)
bind scope = foldM go where
  go :: Env ctx -> CoreFn.Bind CoreFn.Ann -> Eval ctx (Env ctx)
  go env (CoreFn.NonRec _ name e) = do
    val <- eval env e
    pure $ bindEnv [(Qualified scope name, val)] env
  go env (CoreFn.Rec exprs) = do
    -- Using MonadFix would be preferable here, because it lets us use a wider
    -- variety of monads for evaluation, but it is slow, and hard to understand
    -- when we want to support complex recursive binding groups (making the
    -- argument to mfix sufficiently lazy is a challenging exercise).
    -- Instead, we use an unsafe promise type which relies on an IORef: 
    -- we construct an environment of values as lazy promises, and demand their
    -- results as needed by other members of the recursive binding group.
    -- This is safe assuming that every promise is fulfilled exactly once, which
    -- will be the case if we restrict ourselves to monads like ReaderT r IO
    -- whose bind operations evaluate their continuation exactly once.
    let m = Map.fromList [ (Qualified scope name, e) | ((_, name), e) <- exprs ]
    newEnv <- mfixTraverse m \newEnv ->
      let env' = bindEnv (Map.toList newEnv) env
       in \e -> eval env' e
    pure (bindEnv (Map.toList newEnv) env)

mfixTraverse :: Traversable t => t a -> (t b -> a -> Eval ctx b) -> Eval ctx (t b)
mfixTraverse ta f = do
  promises <- liftIO $ traverse (\a -> (a, ) <$> Internal.emptyPromise) ta
  tb <- liftIO $ traverse (Internal.require . snd) promises
  let ftb = f tb
  for_ promises \(a, p) -> do
    b <- ftb a
    liftIO $ Internal.fulfill p b
  pure tb

-- | Apply a value which represents an unevaluated closure to an argument.
apply
  :: Value ctx
  -> Value ctx
  -> Eval ctx (Value ctx)
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
class ToValue ctx a where
  toValue :: a -> Value ctx
  
  -- | The default implementation uses generic deriving to identify a Haskell
  -- record type with a single data constructor with a PureScript record with
  -- the same field names.
  default toValue :: (G.Generic a, ToObject ctx (G.Rep a)) => a -> Value ctx
  toValue = genericToValue defaultObjectOptions
  
  fromValue :: Value ctx -> Eval ctx a
  
  default fromValue :: (G.Generic a, ToObject ctx (G.Rep a)) => Value ctx -> Eval ctx a
  fromValue = genericFromValue defaultObjectOptions
  
instance ctx ~ ctx' => ToValue ctx (Value ctx') where
  toValue = id
  fromValue = pure

-- | The Haskell 'Integer' type corresponds to PureScript's integer type.
instance ToValue ctx Integer where
  {-# INLINE toValue #-}
  toValue = Int
  {-# INLINE fromValue #-}
  fromValue = \case
    Int i -> pure i
    val -> throwErrorWithContext (TypeMismatch "integer" val)
  
-- | The Haskell 'Douvle' type corresponds to the subset of PureScript
-- values consisting of its Number type.
instance ToValue ctx Double where
  {-# INLINE toValue #-}
  toValue = Number
  {-# INLINE fromValue #-}
  fromValue = \case
    Number s -> pure s
    val -> throwErrorWithContext (TypeMismatch "number" val)

-- | The Haskell 'Text' type is represented by PureScript strings
-- which contain no lone surrogates.
instance ToValue ctx Text where
  {-# INLINE toValue #-}
  toValue = String
  {-# INLINE fromValue #-}
  fromValue = \case
    String s -> pure s
    val -> throwErrorWithContext (TypeMismatch "string" val)

-- | The Haskell 'Char' type is represented by PureScript characters.
instance ToValue ctx Char where
  {-# INLINE toValue #-}
  toValue = Char
  {-# INLINE fromValue #-}
  fromValue = \case
    Char c -> pure c
    val -> throwErrorWithContext (TypeMismatch "char" val)

-- | Haskell booleans are represented by boolean values.
instance ToValue ctx Bool where
  {-# INLINE toValue #-}
  toValue = Bool
  {-# INLINE fromValue #-}
  fromValue = \case
    Bool b -> pure b
    val -> throwErrorWithContext (TypeMismatch "boolean" val)
  
-- | Haskell functions are represented as closures which take valid
-- representations for the domain type to valid representations of the codomain
-- type.
instance (ToValue ctx a, ToValueRHS ctx b) => ToValue ctx (a -> b) where
  toValue f = Closure (\v -> toValueRHS . f =<< fromValue v)
  fromValue f = pure $ \a -> fromValueRHS (apply f (toValue a))

-- | Haskell vectors are represented as homogeneous vectors of values, each of
-- which are valid representations of the element type.
instance ToValue ctx a => ToValue ctx (Vector a) where
  {-# INLINE toValue #-}
  toValue = Array . fmap toValue
  {-# INLINE fromValue #-}
  fromValue = \case
    Array xs -> traverse fromValue xs
    val -> throwErrorWithContext (TypeMismatch "array" val)
    
instance (k ~ Text, ToValue ctx a) => ToValue ctx (HashMap k a) where
  {-# INLINE toValue #-}
  toValue = Object . fmap toValue
  {-# INLINE fromValue #-}
  fromValue = \case
    Object o -> traverse fromValue o
    val -> throwErrorWithContext (TypeMismatch "object" val)

-- | This type can be used to make custom Haskell types accessible to 
-- PureScript code via the FFI's @foreign import data@ feature.
newtype ForeignType a = ForeignType { getForeignType :: a }

instance forall ctx a. Typeable a => ToValue ctx (ForeignType a) where
  {-# INLINE toValue #-}
  toValue = Foreign . Dynamic.toDyn . getForeignType
  {-# INLINE fromValue #-}
  fromValue = \case
    Foreign dyn 
      | Just a <- Dynamic.fromDynamic @a dyn -> pure (ForeignType a)
    val -> 
      let typeName = show @TypeRep (typeRep (Proxy :: Proxy a))
       in throwErrorWithContext (TypeMismatch (Text.pack typeName) val)

-- | 'ToValue' should support functions with types such as
--
-- @
-- a -> Eval ctx b
-- a -> b -> Eval ctx c
-- a -> b -> c -> Eval ctx d
-- (a -> Eval ctx b) -> Eval ctx c
-- (a -> b -> Eval ctx c) -> Eval ctx d
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
-- a -> Eval ctx b (b -> Eval ctx c)
-- a -> Eval ctx b (b -> Eval ctx (c -> Eval ctx d))
-- @
--
-- For this reason, the 'ToValue' instance for functions delegates to this
-- type class for the type on the right hand side of the function. It skips the
-- application of 'Eval' for nested function types.
class ToValueRHS ctx a where
  toValueRHS :: a -> Eval ctx (Value ctx)
  fromValueRHS :: Eval ctx (Value ctx) -> a
  
instance (ToValue ctx a, ToValueRHS ctx b) => ToValueRHS ctx (a -> b) where
  {-# INLINE toValueRHS #-}
  toValueRHS f = pure (Closure (\v -> toValueRHS . f =<< fromValue v))
  {-# INLINE fromValueRHS #-}
  fromValueRHS mv a = fromValueRHS do
    v <- mv
    fromValueRHS (apply v (toValue a))
   
instance (ToValue ctx a, ctx ~ ctx') => ToValueRHS ctx (Eval ctx' a) where
  {-# INLINE toValueRHS #-}
  toValueRHS = fmap toValue
  {-# INLINE fromValueRHS #-}
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
  :: (G.Generic a, ToObject ctx (G.Rep a))
  => ObjectOptions
  -> a
  -> Value ctx
genericToValue opts = Object . toObject opts . G.from

-- | Derived 'fromValue' function for Haskell record types which should map to 
-- corresponding PureScript record types.
genericFromValue
  :: (G.Generic a, ToObject ctx (G.Rep a))
  => ObjectOptions
  -> Value ctx
  -> Eval ctx a
genericFromValue opts = \case
  Object o -> G.to <$> fromObject opts o
  val -> throwErrorWithContext (TypeMismatch "object" val)
       
-- | This class is used in the default instance for 'ToValue', via generic
-- deriving, in order to identify a Haskell record type (with a single data
-- constructor and named fields) with values in the semantic domain
-- corresponding to a PureScript record type with the same field names.
class ToObject ctx f where
  toObject :: ObjectOptions -> f x -> HashMap Text (Value ctx)
  fromObject :: ObjectOptions -> HashMap Text (Value ctx) -> Eval ctx (f x)
  
instance ToObject ctx f => ToObject ctx (G.M1 G.D t f) where
  toObject opts = toObject opts . G.unM1
  fromObject opts = fmap G.M1 . fromObject opts
  
instance ToObject ctx f => ToObject ctx (G.M1 G.C t f) where
  toObject opts = toObject opts . G.unM1
  fromObject opts = fmap G.M1 . fromObject opts
  
instance (ToObject ctx f, ToObject ctx g) => ToObject ctx (f G.:*: g) where
  toObject opts (f G.:*: g) = toObject opts f <> toObject opts g
  fromObject opts o = (G.:*:) <$> fromObject opts o <*> fromObject opts o
    
instance 
    forall ctx field u s l r a
     . ( KnownSymbol field
       , ToValue ctx a
       ) 
    => ToObject ctx 
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
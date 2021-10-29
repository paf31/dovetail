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
  ( 
  -- * High-level API
    buildCoreFn
  , builtIn
  
  -- * Evaluation
  -- ** Representation of values
  , Value(..)
  
  -- ** Evaluation monad
  , EvalT(..)
  , runEvalT
  , Eval
  , runEval
  , EvaluationError(..)
  , renderEvaluationError
  
  -- ** Eval/apply
  , Env
  , eval
  , apply
  
  -- ** Foreign function interface
  , FFI(..)
  , ForeignImport(..)
  , toEnv
  , toExterns
  
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
  ) where
 
import Control.Monad (guard, foldM, join, mzero, zipWithM)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans.Class (MonadTrans, lift)
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
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Externs qualified as Externs
import Language.PureScript.Names (Ident(..), Qualified(..))
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString

data FFI m = FFI
  { ffi_moduleName :: P.ModuleName
  , ffi_values :: [ForeignImport m]
  }
  
data ForeignImport m = ForeignImport
  { fv_name :: P.Ident
  , fv_type :: P.SourceType
  , fv_value :: Value m
  }

toExterns :: FFI m -> P.ExternsFile
toExterns (FFI mn vals) =
  Externs.ExternsFile   
    { Externs.efVersion      = "0.14.2"
    , Externs.efModuleName   = mn
    , Externs.efExports      = [P.ValueRef P.nullSourceSpan name | ForeignImport name _ _ <- vals]
    , Externs.efImports      = [ P.ExternsImport (P.ModuleName "Prim") P.Implicit (Just (P.ModuleName "Prim"))
                               , P.ExternsImport (P.ModuleName "Prim") P.Implicit Nothing
                               ]
    , Externs.efFixities     = []
    , Externs.efTypeFixities = []
    , Externs.efDeclarations = [Externs.EDValue name ty | ForeignImport name ty _ <- vals]
    , Externs.efSourceSpan   = P.nullSourceSpan
    } 

toEnv :: FFI m -> Env m
toEnv (FFI mn vals) = 
  Map.fromList [ (P.mkQualified name mn, val) | ForeignImport name _ val <- vals ]
  
-- | Evaluate each of the bindings in a compiled PureScript module, and store
-- the evaluated values in the environment, without evaluating any main
-- expression.
buildCoreFn :: MonadFix m => Env m -> CoreFn.Module ann -> EvalT m (Env m)
buildCoreFn env CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleDecls } = 
  bind moduleName (Just moduleName) env (fmap void moduleDecls)
  
-- | Create an environment from a Haskell value.
--
-- It is recommended that a type annotation is given for the type of the value
-- being provided.
--
-- For example:
--
-- @
--    builtIn "greeting" ("Hello, World!" :: Text)
--    builtIn "somePrimes" ([2, 3, 5, 7, 11] :: Vector Integer)
-- @
--
-- Functions can be provided as built-ins, but the 'EvalT' monad needs to be
-- used to wrap any outputs (or values in positive position):
--
-- @
--    builtIn "strip" ((pure . Text.strip) :: Text -> EvalT m Text)
--    builtIn "map" (traverse :: (Value -> EvalT m Value) -> Vector Value -> EvalT m (Vector Value))
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

-- | The representation of values used by the interpreter - essentially, the
-- semantic domain for a simple untyped lambda calculus with records and ADTs.
--
-- Any additional side effects which might occur in FFI calls to Haskell code
-- are tracked by a monad in the type argument.
data Value m
  = Object (HashMap Text (Value m))
  -- ^ Records are represented as hashmaps from their field names to values
  | Array (Vector (Value m))
  -- ^ Arrays
  | String Text
  | Number Scientific
  -- ^ Numeric values
  -- 
  -- TODO: separate integers from floating-point values
  | Bool Bool
  | Closure (Value m -> EvalT m (Value m))
  -- ^ Closures, represented in higher-order abstract syntax style.
  | Constructor (Qualified (Names.ProperName 'Names.ConstructorName)) [Value m]
  -- ^ Fully-applied data constructors

-- | An environment, i.e. a mapping from names to evaluated values.
--
-- An environment for a single built-in function can be constructed
-- using the 'builtIn' function, and environments can be combined
-- easily using the 'Monoid' instance for 'Map'.
type Env m = Map (Qualified Ident) (Value m)

-- | The monad used by the interpreter, which supports error reporting for errors
-- which can occur during evaluation.
--
-- The transformed monad is used to track any benign side effects that might be
-- exposed via the foreign function interface to PureScript code.
newtype EvalT m a = EvalT { unEvalT :: ExceptT EvaluationError m a }
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadError EvaluationError
    , MonadFix
    )

runEvalT :: EvalT m a -> m (Either EvaluationError a)
runEvalT = runExceptT . unEvalT

-- | Non-transformer version of `EvalT`, useful in any settings where the FFI
-- does not use any side effects during evaluation.
type Eval = EvalT Identity

runEval :: Eval a -> Either EvaluationError a
runEval = runIdentity . runEvalT

-- | Errors which can occur during evaluation of PureScript code.
-- 
-- PureScript is a typed language, and tries to prevent runtime errors.
-- However, in the context of this interpreter, we can receive data from outside
-- PureScript code, so it is possible that runtime errors can occur if we are
-- not careful. This is similar to how PureScript code can fail at runtime
-- due to errors in the FFI.
data EvaluationError 
  = UnknownIdent (Qualified Ident)
  -- ^ A name was not found in the environment
  | TypeMismatch Text
  -- ^ The runtime representation of a value did not match the expected
  -- representation
  | FieldNotFound Text
  -- ^ A record field did not exist in an 'Object' value.
  | InexhaustivePatternMatch
  -- ^ A pattern match failed to match its argument
  | InvalidNumberOfArguments Int Int
  -- ^ A pattern match received the wrong number of arguments
  | UnsaturatedConstructorApplication
  -- ^ A pattern match occurred against a partially-applied data constructor
  | InvalidFieldName PSString.PSString
  -- ^ A PureScript string which contains lone surrogates which could not be
  -- decoded. See 'PSString.PSString'.
  | OtherError Text
  -- ^ An error occurred in a foreign function which is not tracked by
  -- any of the other error types.
  --
  -- TODO: remove this in favor of using monadic FFI functions
  deriving Show

-- | Render an 'EvaluationError' as a human-readable string.
renderEvaluationError :: EvaluationError -> String
renderEvaluationError (UnknownIdent x) =
  "Identifier not in scope: " <> Text.unpack (Names.showQualified Names.showIdent x)
renderEvaluationError (TypeMismatch x) =
  "Type mismatch, expected " <> Text.unpack x
renderEvaluationError (FieldNotFound x) =
  "Record field not found: " <> Text.unpack x
renderEvaluationError InexhaustivePatternMatch =
  "Inexhaustive pattern match"
renderEvaluationError (InvalidNumberOfArguments given expected) =
  "Invalid number of arguments, given " <> show given <> ", but expected " <> show expected
renderEvaluationError UnsaturatedConstructorApplication =
  "Unsaturated constructor application"
renderEvaluationError (InvalidFieldName x) =
  "Invalid field name: " <> PSString.decodeStringWithReplacement x
renderEvaluationError (OtherError x) =
  "Other error: " <> Text.unpack x

evalPSString :: MonadFix m => PSString.PSString -> EvalT m Text
evalPSString pss = 
  case PSString.decodeString pss of
    Just field -> pure field
    _ -> throwError (InvalidFieldName pss)

-- | Evaluate a PureScript CoreFn expression in the given environment.
--
-- Note: it should not be necessary to call this function directly in most
-- circumstances. It is provided as a helper function, for some more advanced
-- use cases, such as setting up a custom environment.
eval 
  :: forall m
   . MonadFix m
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
  result <- runMaybeT (asum (map (match mn env vals) alts))
  case result of
    Nothing -> throwError InexhaustivePatternMatch
    Just (newEnv, matchedExpr) -> eval mn (newEnv <> env) matchedExpr
eval mn _ (CoreFn.Constructor _ _tyName ctor fields) = 
    pure $ go fields []
  where
    go [] applied = Constructor (Qualified (Just mn) ctor) (reverse applied)
    go (_ : tl) applied = Closure \arg -> pure (go tl (arg : applied))

match :: MonadFix m
      => Names.ModuleName
      -> Env m
      -> [Value m]
      -> CoreFn.CaseAlternative ()
      -> MaybeT (EvalT m) (Env m, CoreFn.Expr ())
match mn env vals (CoreFn.CaseAlternative binders expr) 
  | length vals == length binders = do
    newEnv <- fold <$> zipWithM matchOne vals binders
    case expr of
      Left guards -> (newEnv, ) <$> asum (map (uncurry (evalGuard mn env)) guards)
      Right e -> pure (newEnv, e)
  | otherwise = throwError (InvalidNumberOfArguments (length vals) (length binders))

evalGuard
  :: MonadFix m
  => Names.ModuleName
  -> Env m
  -> CoreFn.Guard ()
  -> CoreFn.Expr ()
  -> MaybeT (EvalT m) (CoreFn.Expr ())
evalGuard mn env g e = do
  test <- lift $ eval mn env g
  case test of
    Bool b -> guard b
    _ -> throwError (TypeMismatch "boolean")
  pure e

matchOne 
  :: MonadFix m
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
matchOne (Constructor ctor vals) (CoreFn.ConstructorBinder _ _tyName ctor' bs) 
  | ctor == ctor'
  = if length vals == length bs 
      then fold <$> zipWithM matchOne vals bs
      else throwError UnsaturatedConstructorApplication
matchOne _ _ = mzero

matchLit
  :: forall m
   . MonadFix m
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

evalLit :: MonadFix m => Names.ModuleName -> Env m -> CoreFn.Literal (CoreFn.Expr ()) -> EvalT m (Value m)
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
   . MonadFix m
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
  go env (CoreFn.Rec exprs) = mfix \newEnv -> do
    vals <- flip traverse exprs \((_, name), e) -> do
      val <- eval mn newEnv e
      pure $ Map.singleton (Qualified scope name) val
    pure (fold vals <> env)

-- | Apply a value which represents an unevaluated closure to an argument.
apply
  :: MonadFix m
  => Value m
  -> Value m
  -> EvalT m (Value m)
apply (Closure f) arg = f arg
apply _ _ = throwError (TypeMismatch "closure")

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
--    fromValue . toValue = pure
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
  toValue = Number . fromIntegral
  fromValue = \case
    Number s
      | Right i <- floatingOrInteger @Double s -> pure i
    _ -> throwError (TypeMismatch "integer")
  
-- | The Haskell 'Scientific' type corresponds to the subset of PureScript
-- values consisting of its Int and Number types.
instance MonadFix m => ToValue m Scientific where
  toValue = Number
  fromValue = \case
    Number s -> pure s
    _ -> throwError (TypeMismatch "number")

-- | The Haskell 'Text' type is represented by PureScript strings
-- which contain no lone surrogates.
instance MonadFix m => ToValue m Text where
  toValue = String
  fromValue = \case
    String s -> pure s
    _ -> throwError (TypeMismatch "string")

-- | Haskell booleans are represented by boolean values.
instance MonadFix m => ToValue m Bool where
  toValue = Bool
  fromValue = \case
    Bool b -> pure b
    _ -> throwError (TypeMismatch "boolean")
  
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
    _ -> throwError (TypeMismatch "array")
    
-- | 'ToValue' should support functions with types such as
--
-- @
--    a -> EvalT m b
--    a -> b -> EvalT m c
--    a -> b -> c -> EvalT m d
--    (a -> EvalT m b) -> EvalT m c
--    (a -> b -> EvalT m c) -> EvalT m d
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
--    a -> EvalT m b (b -> EvalT m c)
--    a -> EvalT m b (b -> EvalT m (c -> EvalT m d))
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
  _ -> throwError (TypeMismatch "object")
       
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
        Nothing -> throwError (FieldNotFound field)
        Just v -> G.M1 . G.K1 <$> fromValue v
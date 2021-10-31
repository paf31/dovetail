{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}

module Language.PureScript.Interpreter.Types (
  -- * Evaluation
  -- ** Value types
    Value(..)
  
  -- ** Evaluation monad
  , Env
  , EvalT(..)
  , runEvalT
  , Eval
  , runEval
  
  -- ** Evaluation errors
  , EvaluationError(..)
  , renderEvaluationError
  ) where
  
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Functor.Identity (Identity(..))
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Language.PureScript.Names (Ident(..), Qualified(..))
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString
  
-- | The representation of values used by the interpreter - essentially, the
-- semantic domain for a simple untyped lambda calculus with records and ADTs.
--
-- Any additional side effects which might occur in FFI calls to Haskell code
-- are tracked by a monad in the type argument.
data Value m
  = Object (HashMap Text (Value m))
  -- ^ Records are represented as hashmaps from their field names to values
  | Array (Vector (Value m))
  | String Text
  | Char Char
  | Number Double
  | Int Integer
  | Bool Bool
  | Closure (Value m -> EvalT m (Value m))
  -- ^ Closures, represented in higher-order abstract syntax style.
  | Constructor (Names.ProperName 'Names.ConstructorName) [Value m]
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
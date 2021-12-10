{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}

module Dovetail.Types (
  -- * Evaluation
  -- ** Value types
    Value(..)
  
  -- ** Evaluation monad
  , Env
  , lookupEnv
  , envNames
  , envToMap
  , envFromMap
  , bindEnv
  
  , EvalT(..)
  , runEvalT
  , Eval
  , runEval
  
  -- ** Evaluation errors
  , EvaluationError(..)
  , EvaluationErrorType(..)
  , renderEvaluationError
  , renderEvaluationErrorType
  
  -- ** Evaluation contexts
  , EvaluationContext(..)
  
  -- *** Stack frames
  , EvaluationStackFrame(..)
  , pushStackFrame
  , throwErrorWithContext
  , renderEvaluationStack
  
  -- * Debugging
  , renderValue
  , RenderValueOptions(..)
  , defaultTerminalRenderValueOptions
  ) where
  
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Dynamic (Dynamic)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity(..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Names (Ident(..), Qualified(..))
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString
import System.Console.ANSI.Types qualified as Color

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
  | Foreign Dynamic
  -- ^ Foreign data types

-- | Options when rendering values as strings using 'renderValue'.
data RenderValueOptions = RenderValueOptions
  { colorOutput :: Bool
  -- ^ Should ANSI terminal color codes be emitted
  , maximumDepth :: Maybe Int
  -- ^ The maximum depth of a subexpression to render, or 'Nothing'
  -- to render the entire 'Value'.
  }

-- | Some sensible default rendering options for use on a terminal
-- which supports color.
defaultTerminalRenderValueOptions :: RenderValueOptions
defaultTerminalRenderValueOptions = RenderValueOptions
  { colorOutput = True
  , maximumDepth = Just 1
  }

-- | Render a 'Value' as human-readable text.
--
-- As a general rule, apart from any closures, the rendered text should evaluate
-- to the value you started with (when 'maximumDepth' is not set).
renderValue :: RenderValueOptions -> Value m -> Text
renderValue RenderValueOptions{ colorOutput, maximumDepth } = fst . go 0 where
  go :: Int -> Value m -> (Text, Bool)
  go n _ | maybe False (n >=) maximumDepth = ("⋯", True)
  go _ (String s) = (Text.pack (yellow (show @Text s)), True)
  go _ (Char c) = (Text.pack (yellow (show @Char c)), True)
  go _ (Number d) = (Text.pack (green (show @Double d)), True)
  go _ (Int i) = (Text.pack (green (show @Integer i)), True)
  go _ (Bool True) = (Text.pack (blue "true"), True)
  go _ (Bool False) = (Text.pack (blue "false"), True)
  go _ (Closure{}) = (Text.pack (blue "<closure>"), True)
  go n (Object o) = ( "{ " <> Text.intercalate ", " 
                      [ Text.pack (yellow (show @Text k)) <> ": " <> fst (go (n + 1) x) 
                      | (k, x) <- sortBy (comparing fst) (HashMap.toList o)
                      ] <> " }"
                  , True
                  )
  go n (Array xs) = ( "[ " <> Text.intercalate ", " 
                       [ fst (go (n + 1) x) 
                       | x <- Vector.toList xs
                       ] <> " ]"
                  , True
                  )
  go n (Constructor ctor args) = (Text.unwords (P.runProperName ctor : map (goParens (n + 1)) args), null args)
  go _ (Foreign{}) = (Text.pack (blue "<foreign>"), True)

  goParens :: Int -> Value m -> Text
  goParens n x = 
    case go n x of
      (result, True) -> result
      (result, False) -> "(" <> result <> ")"
      
  color :: (Color.ColorIntensity, Color.Color) -> String -> String
  color c 
    | colorOutput = (Errors.ansiColor c <>) . (<> Errors.ansiColorReset)
    | otherwise = id

  yellow :: String -> String
  yellow = color (Color.Dull, Color.Yellow)

  green :: String -> String
  green = color (Color.Dull, Color.Green)

  blue :: String -> String
  blue = color (Color.Vivid, Color.Blue)
      
-- | An environment, i.e. a mapping from names to evaluated values.
--
-- An environment for a single built-in function can be constructed
-- using the 'builtIn' function, and environments can be combined
-- easily using the 'Monoid' instance for 'Map'.
newtype Env m = Env { _getEnv :: Map (Qualified Ident) (Value m) }
  deriving newtype (Semigroup, Monoid)

lookupEnv :: Qualified Ident -> Env m -> Maybe (Value m)
lookupEnv q (Env env) = Map.lookup q env

envNames :: Env m -> Set (Qualified Ident)
envNames (Env es) = Map.keysSet es

envFromMap :: Map (Qualified Ident) (Value m) -> Env m
envFromMap m = Env m

envToMap :: Env m -> Map (Qualified Ident) (Value m)
envToMap (Env e) = e

bindEnv :: [(Qualified Ident, Value m)] -> Env m -> Env m
bindEnv xs (Env e) = Env (Map.fromList xs <> e)

-- | An evaluation context currently consists of an evaluation stack, which
-- is only used for debugging purposes.
--
-- The context type is parameterized by a monad @m@, because stack frames can
-- contain environments, which can in turn contain 'Value's, which may contain
-- monadic closures. This can be useful for inspecting values or resuming execution
-- in the event of an error.
newtype EvaluationContext m = EvaluationContext 
  { getEvaluationContext :: [EvaluationStackFrame m] }
  
-- | A single evaluation stack frame
-- TODO: support frames for foreign function calls
data EvaluationStackFrame m = EvaluationStackFrame
  { frameEnv :: Env m
  -- ^ The current environment in this stack frame 
  , frameSource :: P.SourceSpan
  -- ^ The source span of the expression whose evaluation created this stack frame.
  , frameExpr :: CoreFn.Expr CoreFn.Ann
  -- ^ The expression whose evaluation created this stack frame.
  }
  
-- | Create a stack frame for the evaluation of an expression, and push it onto
-- the stack.
pushStackFrame :: Monad m => Env m -> CoreFn.Expr CoreFn.Ann -> EvalT m a -> EvalT m a
pushStackFrame env expr = 
    local \(EvaluationContext frames) ->
      EvaluationContext (frame : frames)
  where
    frame = EvaluationStackFrame 
      { frameEnv = env
      , frameSource = let (ss, _, _, _) = CoreFn.extractAnn expr in ss
      , frameExpr = expr
      }

-- | Throw an error which captures the current execution context.
throwErrorWithContext 
  :: ( MonadError (EvaluationError x) m
     , MonadReader (EvaluationContext x) m
     ) 
  => EvaluationErrorType x
  -> m a
throwErrorWithContext errorType = do
  errorContext <- ask
  throwError EvaluationError 
    { errorType
    , errorContext
    }
    
-- | The monad used by the interpreter, which supports error reporting for errors
-- which can occur during evaluation.
--
-- The transformed monad is used to track any benign side effects that might be
-- exposed via the foreign function interface to PureScript code.
newtype EvalT m a = EvalT { unEvalT :: ReaderT (EvaluationContext m) (ExceptT (EvaluationError m) m) a }
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , MonadError (EvaluationError m)
    , MonadReader (EvaluationContext m)
    , MonadIO
    )

instance MonadTrans EvalT where
  lift = EvalT . lift . lift

runEvalT :: EvalT m a -> m (Either (EvaluationError m) a)
runEvalT = runExceptT . flip runReaderT (EvaluationContext []) . unEvalT

-- | Non-transformer version of `EvalT`, useful in any settings where the FFI
-- does not use any side effects during evaluation.
type Eval = EvalT Identity

runEval :: Eval a -> Either (EvaluationError Identity) a
runEval = runIdentity . runEvalT

-- | An evaluation error containing the evaluation context at the point the
-- error was raised.
data EvaluationError m = EvaluationError
  { errorType :: EvaluationErrorType m
  -- ^ The type of error which was raised
  , errorContext :: EvaluationContext m
  -- ^ The evaluation context at the point the error was raised.
  } 

-- | Errors which can occur during evaluation of PureScript code.
-- 
-- PureScript is a typed language, and tries to prevent runtime errors.
-- However, in the context of this interpreter, we can receive data from outside
-- PureScript code, so it is possible that runtime errors can occur if we are
-- not careful. This is similar to how PureScript code can fail at runtime
-- due to errors in the FFI.
data EvaluationErrorType m
  = UnknownIdent (Qualified Ident)
  -- ^ A name was not found in the environment
  | TypeMismatch Text (Value m)
  -- ^ The runtime representation of a value did not match the expected
  -- representation
  | FieldNotFound Text (Value m)
  -- ^ A record field did not exist in an 'Object' value.
  | InexhaustivePatternMatch [Value m]
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

-- | Render an 'EvaluationError' as a human-readable string.
renderEvaluationError :: RenderValueOptions -> EvaluationError m -> String
renderEvaluationError opts (EvaluationError{ errorType, errorContext }) =
  unlines $
    [ maybe "Error"
        (("Error " <>) . Text.unpack . renderEvaluationStackFrame)
        (listToMaybe (getEvaluationContext errorContext))
    ] <>
    [ ""
    , "  " <> renderEvaluationErrorType opts errorType
    , ""
    , "In context:"
    ] <> concat
    [ [ "  " <> Text.unpack (Names.showIdent (P.disqualify ident))
      , "  = " <> Text.unpack (renderValue opts value)
      , ""
      ]
    | headFrame <- take 1 (getEvaluationContext errorContext)
    , (ident, value) <- Map.toList (envToMap (frameEnv headFrame))
    , P.isUnqualified ident
    ] <> 
    [ Text.unpack 
        (renderEvaluationStack (drop 1 (getEvaluationContext errorContext)))
    ]
  
renderEvaluationStack :: [EvaluationStackFrame m] -> Text
renderEvaluationStack frames =
  Text.unlines
    [ renderEvaluationStackFrame frame
    | frame <- frames 
    ]

renderEvaluationStackFrame :: EvaluationStackFrame m -> Text
renderEvaluationStackFrame frame =
  "at " <> fold
    [ P.displaySourcePos (P.spanStart (frameSource frame)) 
    , " - " 
    , P.displaySourcePos (P.spanEnd (frameSource frame))
    ]
  
renderEvaluationErrorType :: RenderValueOptions -> EvaluationErrorType m -> String
renderEvaluationErrorType _ (UnknownIdent x) =
  "Identifier not in scope: " <> Text.unpack (Names.showQualified Names.showIdent x)
renderEvaluationErrorType opts (TypeMismatch x val) =
  "Type mismatch, expected " <> Text.unpack x <> ", but got value " <> Text.unpack (renderValue opts val)
renderEvaluationErrorType opts (FieldNotFound x val) =
  "Record field " <> show x <> " was not present in value: " <> Text.unpack (renderValue opts val)
renderEvaluationErrorType _ InexhaustivePatternMatch{} =
  "Inexhaustive pattern match"
renderEvaluationErrorType _ (InvalidNumberOfArguments given expected) =
  "Invalid number of arguments, given " <> show given <> ", but expected " <> show expected
renderEvaluationErrorType _ UnsaturatedConstructorApplication =
  "Unsaturated constructor application"
renderEvaluationErrorType _ (InvalidFieldName x) =
  "Invalid field name: " <> PSString.decodeStringWithReplacement x
renderEvaluationErrorType _ (OtherError x) =
  "Other error: " <> Text.unpack x
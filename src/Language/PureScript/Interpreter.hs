{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.PureScript.Interpreter
  ( module Language.PureScript.Interpreter.Types
  , module Language.PureScript.Interpreter.FFI
  , module Language.PureScript.Interpreter.FFI.Builder
  
  -- * High-level API
  , InterpretT
  , runInterpretT
  , runInterpret
  
  -- ** Error messages
  , InterpretError(..)
  , renderInterpretError
  
  -- ** Foreign function interface
  , ffi
  
  -- ** Building PureScript source
  , build
  , buildCoreFn
  
  -- ** Evaluating values
  , eval
  , evalMain
  
  , module Language.PureScript.Interpreter.Evaluate
  , module Language.PureScript.Make.Simplified
  , module Language.PureScript.CoreFn
  , module Language.PureScript.Names
  ) where
 
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.State (StateT, evalStateT, get, put, modify)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity(..))
import Data.Text (Text)
import Language.PureScript qualified as P
import Language.PureScript.CoreFn (Ann, Expr, Module)
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Interpreter.Evaluate (Env, 
                                                 EvalT(..), runEvalT, 
                                                 Eval, runEval, 
                                                 ToValue(..), ToValueRHS(..))
import Language.PureScript.Interpreter.Evaluate qualified as Evaluate
import Language.PureScript.Interpreter.FFI
import Language.PureScript.Interpreter.FFI qualified as FFI
import Language.PureScript.Interpreter.FFI.Builder
import Language.PureScript.Interpreter.Types 
import Language.PureScript.Make.Simplified (BuildError(..), renderBuildError)
import Language.PureScript.Make.Simplified qualified as Make
import Language.PureScript.Names

-- | A monad transformer for high-level tasks involving PureScript code, including separate 
-- compilation. Its job is to keep track of available modules, any foreign imports
-- from Haskell code, and run PureScript code.
--
-- Note: do not confuse this monad transformer with 'EvalT', which is only
-- responsible for powering evaluation of PureScript expressions.
--
-- The transformed monad is used to track any benign side effects that might be
-- exposed via the foreign function interface to PureScript code, in the same sense
-- as 'EvalT'.
newtype InterpretT m a = InterpretT { unInterpretT :: StateT ([P.ExternsFile], Env m) (ExceptT InterpretError m) a }
  deriving newtype (Functor, Applicative, Monad)
  
instance MonadTrans InterpretT where
  lift = InterpretT . lift . lift

-- | Run a computation in the 'InterpretT' monad, possibly returning an error.
-- Note: errors can occur during module building or evaluation (i.e. module loading).
--
-- The 'runInterpret' function is a simpler alternative in the case where benign
-- side-effects are not needed.
--
-- For example:
--
-- @
-- runInterpret @Module do
--   -- Load the prelude
--   'ffi' 'prelude'
--   -- Build a module from source
--   'build' "module Main where main = \\\"example\\\"" --
--
-- runInterpret @(Eval Text) do
--   'ffi' 'prelude'
--   _ <- 'build' "module Main where main = \\\"example\\\""
--   -- Evaluate the main function
--   'evalMain' ('P.ModuleName' \"Main\")
-- @
runInterpretT :: Monad m => InterpretT m a -> m (Either InterpretError a)
runInterpretT = runExceptT . flip evalStateT ([], mempty) . unInterpretT

type Interpret = InterpretT Identity

runInterpret :: Interpret a -> Either InterpretError a
runInterpret = runIdentity . runInterpretT

-- | Make an 'FFI' module available for use to subsequent operations.
--
-- For example, to make the 'Language.PureScript.Interpreter.Prelude.prelude' available:
--
-- @
-- ffi 'Language.PureScript.Interpreter.Prelude.prelude'
-- @
ffi :: Monad m => FFI m -> InterpretT m ()
ffi f = InterpretT $ modify \(externs, env) -> 
  ( FFI.toExterns f : externs
  , env <> FFI.toEnv f
  )

-- | The type of errors that can occur in the 'InterpretT' monad.
data InterpretError
  = EvaluationError Evaluate.EvaluationError
  -- ^ Evaluation errors can occur during the initial evaluation of the module
  -- when it is loaded into the environment.
  | BuildError Make.BuildError
  -- ^ Build errors can occur if we are building modules from source or corefn.
  deriving Show

renderInterpretError :: InterpretError -> String
renderInterpretError (BuildError err) =
  "Build error: " <> Make.renderBuildError err
renderInterpretError (EvaluationError err) =
  "Evaluation error: " <> Evaluate.renderEvaluationError err

liftWith :: Monad m => (e -> InterpretError) -> m (Either e a) -> InterpretT m a
liftWith f ma = InterpretT . lift . ExceptT $ fmap (first f) ma

-- | Build a PureScript module from source, and make its exported functions available
-- during subsequent evaluations.
build :: MonadFix m => Text -> InterpretT m (CoreFn.Module CoreFn.Ann)
build moduleText = do
  (externs, env) <- InterpretT get
  (m, newExterns) <- liftWith BuildError $ pure $ Make.buildSingleModule externs moduleText
  InterpretT $ put (newExterns : externs, env)
  buildCoreFn m

-- | Build a PureScript module from corefn, and make its exported functions available
-- during subsequent evaluations.
--
-- The corefn module may be preprepared, for example by compiling from source text using the
-- functions in the "Language.PureScript.Make.Simplified" module.
buildCoreFn :: MonadFix m => CoreFn.Module CoreFn.Ann -> InterpretT m (CoreFn.Module CoreFn.Ann)
buildCoreFn m = do
  (externs, env) <- InterpretT get
  newEnv <- liftWith EvaluationError (Evaluate.runEvalT (Evaluate.buildCoreFn env m))
  InterpretT $ put (externs, newEnv)
  pure m

-- | Evaluate a PureScript corefn expression and return the result.
-- Note: The expression is not type-checked by the PureScript typechecker. 
-- See the documentation for 'ToValueRHS' for valid result types.
eval :: (MonadFix m, ToValueRHS m a) => CoreFn.Expr () -> InterpretT m a
eval expr = do
  (_externs, env) <- InterpretT get
  pure . Evaluate.fromValueRHS $ Evaluate.eval env expr

-- | Evaluate @main@ in the specified module and return the result.
evalMain :: (MonadFix m, ToValueRHS m a) => P.ModuleName -> InterpretT m a
evalMain moduleName = eval (CoreFn.Var () (P.Qualified (Just moduleName) (P.Ident "main")))
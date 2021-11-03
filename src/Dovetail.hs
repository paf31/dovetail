{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Dovetail
  ( module Dovetail.Types
  , module Dovetail.FFI
  , module Dovetail.FFI.Builder
  
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
  
  -- ** Evaluating expressions
  , eval
  , evalCoreFn
  , evalMain
  
  , module Dovetail.Evaluate
  , module Dovetail.Build
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
import Dovetail.Build (BuildError(..), renderBuildError)
import Dovetail.Build qualified as Make
import Dovetail.Evaluate (Env, EvalT(..), runEvalT, Eval, runEval, 
                          ToValue(..), ToValueRHS(..))
import Dovetail.Evaluate qualified as Evaluate
import Dovetail.FFI
import Dovetail.FFI qualified as FFI
import Dovetail.FFI.Builder
import Dovetail.Types 
import Language.PureScript qualified as P
import Language.PureScript.CoreFn (Ann, Expr, Module)
import Language.PureScript.CoreFn qualified as CoreFn
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
newtype InterpretT m a = InterpretT { unInterpretT :: StateT ([P.ExternsFile], Env m) (ExceptT (InterpretError m) m) a }
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
runInterpretT :: Monad m => InterpretT m a -> m (Either (InterpretError m) a)
runInterpretT = runExceptT . flip evalStateT ([], mempty) . unInterpretT

type Interpret = InterpretT Identity

runInterpret :: Interpret a -> Either (InterpretError Identity) a
runInterpret = runIdentity . runInterpretT

-- | Make an 'FFI' module available for use to subsequent operations.
--
-- For example, to make the 'Dovetail.Prelude.prelude' available:
--
-- @
-- ffi 'Dovetail.Prelude.prelude'
-- @
ffi :: Monad m => FFI m -> InterpretT m ()
ffi f = InterpretT $ modify \(externs, env) -> 
  ( FFI.toExterns f : externs
  , env <> FFI.toEnv f
  )

-- | The type of errors that can occur in the 'InterpretT' monad.
data InterpretError m
  = ErrorDuringEvaluation (Evaluate.EvaluationError m)
  -- ^ Evaluation errors can occur during the initial evaluation of the module
  -- when it is loaded into the environment.
  | ErrorDuringBuild Make.BuildError
  -- ^ Build errors can occur if we are building modules from source or corefn.

renderInterpretError :: InterpretError m -> String
renderInterpretError (ErrorDuringBuild err) =
  "Build error: " <> Make.renderBuildError err
renderInterpretError (ErrorDuringEvaluation err) =
  "Evaluation error: " <> Evaluate.renderEvaluationError err

liftWith :: Monad m => (e -> InterpretError m) -> m (Either e a) -> InterpretT m a
liftWith f ma = InterpretT . lift . ExceptT $ fmap (first f) ma

-- | Build a PureScript module from source, and make its exported functions available
-- during subsequent evaluations.
build :: MonadFix m => Text -> InterpretT m (CoreFn.Module CoreFn.Ann)
build moduleText = do
  (externs, _) <- InterpretT get
  (m, newExterns) <- liftWith ErrorDuringBuild $ pure $ Make.buildSingleModule externs moduleText
  buildCoreFn newExterns m

-- | Build a PureScript module from corefn, and make its exported functions available
-- during subsequent evaluations.
--
-- The corefn module may be preprepared, for example by compiling from source text using the
-- functions in the "Dovetail.Build" module.
buildCoreFn :: MonadFix m => P.ExternsFile -> CoreFn.Module CoreFn.Ann -> InterpretT m (CoreFn.Module CoreFn.Ann)
buildCoreFn newExterns m = do
  (externs, env) <- InterpretT get
  newEnv <- liftWith ErrorDuringEvaluation (Evaluate.runEvalT (Evaluate.buildCoreFn env m))
  InterpretT $ put (newExterns : externs, newEnv)
  pure m

-- | Evaluate a PureScript expression from source
eval
  :: (MonadFix m, ToValueRHS m a)
  => Maybe P.ModuleName
  -- ^ The name of the "default module" whose exports will be made available unqualified
  -- to the evaluated expression.
  -> Text
  -> InterpretT m (a, P.SourceType)
eval defaultModule exprText = do
  (externs, env) <- InterpretT get
  (expr, ty) <- liftWith ErrorDuringBuild $ pure $ Make.buildSingleExpression defaultModule externs exprText
  pure (Evaluate.fromValueRHS (Evaluate.eval env expr), ty)

-- | Evaluate a PureScript corefn expression and return the result.
-- Note: The expression is not type-checked by the PureScript typechecker. 
-- See the documentation for 'ToValueRHS' for valid result types.
evalCoreFn :: (MonadFix m, ToValueRHS m a) => CoreFn.Expr CoreFn.Ann -> InterpretT m a
evalCoreFn expr = do
  (_externs, env) <- InterpretT get
  pure . Evaluate.fromValueRHS $ Evaluate.eval env expr

-- | Evaluate @main@ in the specified module and return the result.
evalMain :: (MonadFix m, ToValueRHS m a) => P.ModuleName -> InterpretT m a
evalMain moduleName = evalCoreFn (CoreFn.Var (CoreFn.ssAnn P.nullSourceSpan) (P.Qualified (Just moduleName) (P.Ident "main")))
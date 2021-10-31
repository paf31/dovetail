{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.PureScript.Interpreter
  ( module Language.PureScript.Interpreter.Types
  , module Language.PureScript.Interpreter.FFI
  , module Language.PureScript.Interpreter.FFI.Builder
  , module Language.PureScript.CoreFn
  , module Language.PureScript.Names
  
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

newtype InterpretT m a = InterpretT { unInterpretT :: StateT ([P.ExternsFile], Env m) (ExceptT InterpretError m) a }
  deriving newtype (Functor, Applicative, Monad)
  
instance MonadTrans InterpretT where
  lift = InterpretT . lift . lift
  
type Interpret = InterpretT Identity

runInterpret :: Interpret a -> Either InterpretError a
runInterpret = runIdentity . runInterpretT

runInterpretT :: Monad m => InterpretT m a -> m (Either InterpretError a)
runInterpretT = runExceptT . flip evalStateT ([], mempty) . unInterpretT

ffi :: Monad m => FFI m -> InterpretT m ()
ffi f = InterpretT $ modify \(externs, env) -> 
  ( FFI.toExterns f : externs
  , env <> FFI.toEnv f
  )

data InterpretError
  = EvaluationError Evaluate.EvaluationError
  | BuildError Make.BuildError
  deriving Show

renderInterpretError :: InterpretError -> String
renderInterpretError (BuildError err) =
  "Build error: " <> Make.renderBuildError err
renderInterpretError (EvaluationError err) =
  "Evaluation error: " <> Evaluate.renderEvaluationError err

liftWith :: Monad m => (e -> InterpretError) -> m (Either e a) -> InterpretT m a
liftWith f ma = InterpretT . lift . ExceptT $ fmap (first f) ma

build :: MonadFix m => Text -> InterpretT m (CoreFn.Module CoreFn.Ann)
build moduleText = do
  (externs, env) <- InterpretT get
  (m, newExterns) <- liftWith BuildError $ pure $ Make.buildSingleModule externs moduleText
  InterpretT $ put (newExterns : externs, env)
  buildCoreFn m

buildCoreFn :: MonadFix m => CoreFn.Module CoreFn.Ann -> InterpretT m (CoreFn.Module CoreFn.Ann)
buildCoreFn m = do
  (externs, env) <- InterpretT get
  newEnv <- liftWith EvaluationError (Evaluate.runEvalT (Evaluate.buildCoreFn env m))
  InterpretT $ put (externs, newEnv)
  pure m

eval :: (MonadFix m, ToValueRHS m a) => CoreFn.Expr () -> InterpretT m a
eval expr = do
  (_externs, env) <- InterpretT get
  pure . Evaluate.fromValueRHS $ Evaluate.eval env expr
    
evalMain :: (MonadFix m, ToValueRHS m a) => P.ModuleName -> InterpretT m a
evalMain moduleName = eval (CoreFn.Var () (P.Qualified (Just moduleName) (P.Ident "main")))
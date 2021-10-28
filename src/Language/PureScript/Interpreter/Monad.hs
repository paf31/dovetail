{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.PureScript.Interpreter.Monad
  ( InterpretT
  , runInterpretT
  , runInterpret
  , ffi
  , BuildError
  , renderBuildError
  , build
  , eval
  , evalMain
  
  -- TODO: reexports
  ) where
 
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.State (StateT, evalStateT, get, put, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity(..))
import Data.Text (Text)
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Interpreter (Env, FFI, ToValueRHS)
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Make.Simplified qualified as Make

newtype InterpretT m a = InterpretT { unInterpretT :: StateT ([P.ExternsFile], Env m) m a }
  deriving newtype (Functor, Applicative, Monad)
  
instance MonadTrans InterpretT where
  lift = InterpretT . lift
  
runInterpret :: Interpret a -> a
runInterpret = runIdentity . runInterpretT
  
type Interpret = InterpretT Identity

runInterpretT :: Monad m => InterpretT m a -> m a
runInterpretT = flip evalStateT ([], mempty) . unInterpretT

ffi :: Monad m => FFI m -> InterpretT m ()
ffi f = InterpretT $ modify \(externs, env) -> 
  ( Interpreter.toExterns f : externs
  , env <> Interpreter.toEnv f
  )

data BuildError
  = EvaluationError Interpreter.EvaluationError
  | BuildError Make.BuildError

renderBuildError :: BuildError -> String
renderBuildError (BuildError err) =
  "Build error: " <> Make.renderBuildError err
renderBuildError (EvaluationError err) =
  "Evaluation error: " <> Interpreter.renderEvaluationError err

build :: MonadFix m => Text -> InterpretT m (Either BuildError (CoreFn.Module CoreFn.Ann))
build moduleText = do
  (externs, env) <- InterpretT get
  runExceptT do
    (m, newExts) <- ExceptT . pure . first BuildError $ Make.buildSingleModule externs moduleText
    newEnv <- ExceptT . lift $ first EvaluationError <$> Interpreter.runEvalT (Interpreter.build env m)
    lift . InterpretT $ put (newExts : externs, newEnv)
    pure m

eval :: (MonadFix m, ToValueRHS m a) => CoreFn.Expr () -> InterpretT m a
eval expr = do
  (_externs, env) <- InterpretT get
  pure . Interpreter.fromValueRHS $
    Interpreter.eval (error "moduleName") env expr
    
evalMain :: (MonadFix m, ToValueRHS m a) => P.ModuleName -> InterpretT m a
evalMain moduleName = eval (CoreFn.Var () (P.Qualified (Just moduleName) (P.Ident "main")))
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Dovetail
  ( module Dovetail.Types
  , module Dovetail.FFI
  , module Dovetail.FFI.Builder
  
  -- * High-level API
  , Interpret
  , runInterpret
  , liftEval
  
  -- ** Debugging
  , runInterpretWithDebugger
  
  -- ** Error messages
  , InterpretError(..)
  , renderInterpretError
  
  -- ** Foreign function interface
  , ffi
  , loadEnv
  
  -- ** Building PureScript source
  , build
  , buildCoreFn
  , module Dovetail.Build
  
  -- ** Evaluating expressions
  , eval
  , evalCoreFn
  , evalMain
  , module Dovetail.Evaluate
  
  -- ** REPL
  , repl
  
  -- * Re-exports
  , module Language.PureScript.CoreFn
  , module Language.PureScript.Names
  ) where

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT, get, put, modify, runStateT)
import Data.Bifunctor (first)
import Data.Set qualified as Set
import Data.Text (Text)
import Dovetail.Build (BuildError(..), renderBuildError)
import Dovetail.Build qualified as Build
import Dovetail.Evaluate (Env, Eval(..), runEval, ToValue(..), ToValueRHS(..))
import Dovetail.Evaluate qualified as Evaluate
import Dovetail.FFI
import Dovetail.FFI qualified as FFI
import Dovetail.FFI.Builder
import Dovetail.REPL qualified as REPL
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
newtype Interpret ctx a = Interpret { unInterpret :: ReaderT ctx (ExceptT (InterpretError ctx) (StateT ([P.ExternsFile], Env ctx) IO)) a }
  deriving newtype (Functor, Applicative, Monad, MonadError (InterpretError ctx), MonadReader ctx, MonadIO)
  
-- | Run a computation in the 'Interpret' monad, possibly returning an error.
-- Note: errors can occur during module building or evaluation (i.e. module loading).
--
-- The 'runInterpret' function is a simpler alternative in the case where benign
-- side-effects are not needed.
--
-- For example:
--
-- @
-- runInterpret @Module () do
--   -- Load the prelude
--   'ffi' 'prelude'
--   -- Build a module from source
--   'build' "module Main where main = \\\"example\\\"" --
--
-- runInterpret @(Eval Text) () do
--   'ffi' 'prelude'
--   _ <- 'build' "module Main where main = \\\"example\\\""
--   -- Evaluate the main function
--   'evalMain' ('P.ModuleName' \"Main\")
-- @
runInterpret :: ctx -> Interpret ctx a -> IO (Either (InterpretError ctx) a)
runInterpret ctx = flip evalStateT ([], mempty) . runExceptT . flip runReaderT ctx . unInterpret

-- | Like 'runInterpret', but starts an interactive debugging session in the
-- event of a debugging error.
runInterpretWithDebugger 
  :: ctx
  -> Interpret ctx a
  -> IO ()
runInterpretWithDebugger ctx x = do
  (e, (externs, env)) <- flip runStateT ([], mempty) $ runExceptT (runReaderT (unInterpret x) ctx)
  case e of
    Left err -> do
      liftIO . putStrLn $ renderInterpretError defaultTerminalRenderValueOptions err
      case err of
        ErrorDuringEvaluation evalErr -> do
          liftIO . putStrLn $ "\nStarting the debugger. ^C to exit."
          let withEnvAtError =
                case errorContext evalErr of
                  EvaluationContext (frame : _) _ ->
                    (frameEnv frame <> env)
                  _ -> env
              additionalNames = 
                [ P.disqualify ident
                | ident <- Set.toList (envNames withEnvAtError Set.\\ envNames env)
                , not (P.isQualified ident)
                ]
          REPL.defaultMain Nothing externs additionalNames withEnvAtError ctx
        _ -> pure ()
    Right{} -> pure ()

-- | A convenience function for running 'EvalT' computations in 'Interpret',
-- reporting errors via 'InterpretError'.
liftEval :: Eval ctx a -> Interpret ctx a
liftEval e = do
  ctx <- ask
  x <- liftIO $ runEval ctx e
  case x of
    Left err -> throwError (ErrorDuringEvaluation err)
    Right a -> pure a

-- | Make an 'FFI' module available for use to subsequent operations.
--
-- For example, to make the 'Dovetail.Prelude.prelude' available:
--
-- @
-- ffi 'Dovetail.Prelude.prelude'
-- @
ffi :: FFI ctx -> Interpret ctx ()
ffi f = Interpret . lift . lift $ modify \(externs, env) -> 
  ( FFI.toExterns f : externs
  , env <> FFI.toEnv f
  )

loadEnv :: Env ctx -> Interpret ctx ()
loadEnv env = Interpret . lift . lift $ modify \(externs, env') -> 
  ( externs
  , env <> env'
  )
  
-- | The type of errors that can occur in the 'Interpret' monad.
data InterpretError ctx
  = ErrorDuringEvaluation (Evaluate.EvaluationError ctx)
  -- ^ Evaluation errors can occur during the initial evaluation of the module
  -- when it is loaded into the environment.
  | ErrorDuringBuild Build.BuildError
  -- ^ Build errors can occur if we are building modules from source or corefn.

renderInterpretError :: RenderValueOptions -> InterpretError ctx -> String
renderInterpretError _ (ErrorDuringBuild err) =
  "Build error: " <> Build.renderBuildError err
renderInterpretError opts (ErrorDuringEvaluation err) =
  "Evaluation error: " <> Evaluate.renderEvaluationError opts err

liftWith :: (e -> InterpretError ctx) -> IO (Either e a) -> Interpret ctx a
liftWith f ma = Interpret . lift . ExceptT . lift $ fmap (first f) ma

-- | Build a PureScript module from source, and make its exported functions available
-- during subsequent evaluations.
build :: Text -> Interpret ctx (CoreFn.Module CoreFn.Ann)
build moduleText = do
  (externs, _) <- Interpret ((lift . lift) get)
  (m, newExterns) <- liftWith ErrorDuringBuild $ pure $ Build.buildSingleModule externs moduleText
  buildCoreFn newExterns m

-- | Build a PureScript module from corefn, and make its exported functions available
-- during subsequent evaluations.
--
-- The corefn module may be preprepared, for example by compiling from source text using the
-- functions in the "Dovetail.Build" module.
buildCoreFn :: P.ExternsFile -> CoreFn.Module CoreFn.Ann -> Interpret ctx (CoreFn.Module CoreFn.Ann)
buildCoreFn newExterns m = do
  ctx <- ask
  (externs, env) <- Interpret ((lift . lift) get)
  newEnv <- liftWith ErrorDuringEvaluation (Evaluate.runEval ctx (Evaluate.buildCoreFn env m))
  Interpret . lift . lift $ put (externs <> [newExterns], newEnv)
  pure m

-- | Evaluate a PureScript expression from source
eval
  :: ToValueRHS ctx a
  => Maybe P.ModuleName
  -- ^ The name of the "default module" whose exports will be made available unqualified
  -- to the evaluated expression.
  -> Text
  -> Interpret ctx (a, P.SourceType)
eval defaultModule exprText = do
  (externs, env) <- Interpret ((lift . lift) get)
  (expr, ty) <- liftWith ErrorDuringBuild $ pure $ Build.buildSingleExpression defaultModule externs exprText
  pure (Evaluate.fromValueRHS (Evaluate.eval env expr), ty)

-- | Evaluate a PureScript corefn expression and return the result.
-- Note: The expression is not type-checked by the PureScript typechecker. 
-- See the documentation for 'ToValueRHS' for valid result types.
evalCoreFn :: ToValueRHS ctx a => CoreFn.Expr CoreFn.Ann -> Interpret ctx a
evalCoreFn expr = do
  (_externs, env) <- Interpret ((lift . lift) get)
  pure . Evaluate.fromValueRHS $ Evaluate.eval env expr

-- | Evaluate @main@ in the specified module and return the result.
evalMain :: ToValueRHS ctx a => P.ModuleName -> Interpret ctx a
evalMain moduleName = evalCoreFn (CoreFn.Var (CoreFn.ssAnn P.nullSourceSpan) (P.Qualified (Just moduleName) (P.Ident "main")))

-- | Start an interactive debugger (REPL) session.
repl 
  :: Maybe P.ModuleName 
  -- ^ The default module, whose members will be available unqualified in scope
  -> Interpret ctx ()
repl defaultModule = do
  (externs, env) <- Interpret ((lift . lift) get)
  ctx <- ask
  liftIO $ REPL.defaultMain defaultModule externs [] env ctx
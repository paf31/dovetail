{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Dovetail.REPL (defaultMain) where

import Control.Monad.Trans.Class (lift)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Dovetail.Build qualified as Build
import Dovetail.Evaluate qualified as Evaluate
import Dovetail.Types
import Language.PureScript qualified as P
import Language.PureScript.AST.Binders qualified as AST
import Language.PureScript.AST.Declarations qualified as AST
import Language.PureScript.CoreFn qualified as CoreFn
import System.Console.Haskeline

renderOptions :: RenderValueOptions
renderOptions = RenderValueOptions
  { colorOutput = True
  , maximumDepth = Nothing
  }

-- | Starts a minimal interactive debugger (REPL) session. 
--
-- It is more likely that you will want to use the 'Dovetail.repl' function to
-- start a REPL session from within an 'Dovetail.InterpretT' block.
defaultMain 
  :: forall ctx
   . Maybe P.ModuleName
  -- ^ The default module, whose members will be available unqualified in scope.
  -> [P.ExternsFile]
  -- ^ Any externs files to load
  -> [P.Ident]
  -- ^ Any additional identifiers which are available in the environment, but not
  -- given types in the externs file. These will be made available without type
  -- information, for debugging purposes.
  -> Env ctx
  -- ^ The evaluation environment
  -> ctx
  -- ^ Any additional context
  -> IO ()
defaultMain defaultModule externs additionalIdentsInScope env ctx = 
    runInputT settings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> do
          case Build.buildSingleExpressionWith abstractAdditionalInputs defaultModule externs (Text.pack input) of
            Right (expr, _) -> do
              let appliedExpr = applyAdditionalInputs expr
              mresult <- lift . runEval ctx $ Evaluate.eval env appliedExpr
              case mresult of
                Right result ->
                  outputStrLn . Text.unpack $ renderValue renderOptions result
                Left err ->
                  outputStrLn $ renderEvaluationError renderOptions err
            Left err ->
              outputStrLn $ Build.renderBuildError err
          loop
        
    -- Since we might have additional identifiers in scope which are not defined
    -- in the externs files (for example, if we stopped at an error), we need to
    -- introduce those names into scope another way, without running afoul of the
    -- typechecker. We do this by binding them to the arguments of a temporary
    -- function, typechecking _that_ function, and applying it in the evaluator
    -- after type checking is complete.
    abstractAdditionalInputs expr =
      foldl (\e name -> 
        AST.Abs (AST.VarBinder P.nullSourceSpan name) e)
        expr 
        additionalIdentsInScope
    
    applyAdditionalInputs expr =
      foldl (\e name -> 
        CoreFn.App (CoreFn.ssAnn P.nullSourceSpan) e 
         (CoreFn.Var (CoreFn.ssAnn P.nullSourceSpan)
           (P.Qualified Nothing name))) 
        expr 
        additionalIdentsInScope
          
    settings = setComplete completionFunc defaultSettings
    
    completionFunc = completeWord Nothing " \t" \s ->
      pure 
        [ simpleCompletion (Text.unpack ident)
        | ident <- allCompletions
        , Text.isPrefixOf (Text.pack s) ident
        ]
      
    allCompletions = map (P.showQualified P.showIdent) (Set.toList (envNames env))
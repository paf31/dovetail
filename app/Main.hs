{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Fix (MonadFix)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Language.PureScript.Interpreter (EvalT, Eval, runEval)
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Make.Simplified qualified as Make
import Language.PureScript.Interpreter.JSON (JSON(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stdin)

initialEnv 
  :: forall m
   . MonadFix m
  => Interpreter.Env m
initialEnv = Map.unions
    [ Interpreter.builtIn "append" append
    , Interpreter.builtIn "map" _map
    , Interpreter.builtIn "appendString" appendString
    , Interpreter.builtIn "not" _not
    ]
  where
    append :: Vector (Interpreter.Value m)
           -> Vector (Interpreter.Value m)
           -> EvalT m (Vector (Interpreter.Value m))
    append xs ys = pure (xs <> ys)
    
    _map :: (Interpreter.Value m -> EvalT m (Interpreter.Value m))
         -> Vector (Interpreter.Value m)
         -> EvalT m (Vector (Interpreter.Value m))
    _map = traverse
    
    appendString :: Text -> Text -> EvalT m Text
    appendString xs ys = pure (xs <> ys)
    
    _not :: Bool -> EvalT m Bool
    _not b = pure (not b)
      
main :: IO ()
main = do
  [_, exprFile] <- getArgs
  exprText <- Text.readFile exprFile
  buildResult <- Make.buildSingleExpression exprFile exprText
  case buildResult of
    Left err -> putStrLn (Make.renderBuildError err) *> exitFailure
    Right m -> do
      stdinBytes <- BL.hGetContents stdin
      case Aeson.eitherDecode stdinBytes of
        Left err -> putStrLn err *> exitFailure
        Right input -> do
          let f :: Eval (JSON Aeson.Value -> Eval (JSON Aeson.Value))
              f = Interpreter.interpret initialEnv m
          case runEval (f >>= ($ JSON input)) of
            Left err -> putStrLn (Interpreter.renderEvaluationError err) *> exitFailure
            Right output ->
              BL8.putStrLn (Pretty.encodePretty (getJSON output))
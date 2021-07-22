{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Error.Class (MonadError)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Make.Simplified qualified as Make
import Language.PureScript.Interpreter.JSON (JSON(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stdin)

initialEnv 
  :: forall m
   . MonadError Interpreter.EvaluationError m
  => Interpreter.Env m
initialEnv = Map.unions
  [ 
  -- Arrays
    Interpreter.builtIn "append" \xs ys ->
      pure ((xs <> ys) :: Vector (Interpreter.Value m))
  , Interpreter.builtIn "map" \f xs ->
      traverse
        (f :: Interpreter.Value m -> m (Interpreter.Value m))
        (xs :: Vector (Interpreter.Value m))
  
  -- Strings
  , Interpreter.builtIn "appendString" \s1 s2 ->
      pure ((s1 <> s2) ::Text)
      
  -- Booleans
  , Interpreter.builtIn "not" (pure . not)
  ]
      
main :: IO ()
main = do
  [_, moduleFile] <- getArgs
  moduleText <- Text.readFile moduleFile
  buildResult <- Make.buildSingleModule moduleFile moduleText
  case buildResult of
    Left err -> putStrLn (Make.renderBuildError err) *> exitFailure
    Right m -> do
      stdinBytes <- BL.hGetContents stdin
      case Aeson.eitherDecode stdinBytes of
        Left err -> putStrLn err *> exitFailure
        Right input -> do
          let f :: Either Interpreter.EvaluationError (JSON Aeson.Value -> Either Interpreter.EvaluationError (JSON Aeson.Value))
              f = Interpreter.interpret initialEnv m
          case f >>= ($ JSON input) of
            Left err -> putStrLn (Interpreter.renderEvaluationError err) *> exitFailure
            Right output ->
              BL8.putStrLn (Pretty.encodePretty (getJSON output))
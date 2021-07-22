{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Functor.Identity (Identity(..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Make.Simplified qualified as Make
import Language.PureScript.Interpreter.JSON (JSON(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stdin)

initialEnv :: Interpreter.Env Identity
initialEnv = Map.unions
  [ 
  -- Arrays
    Interpreter.builtIn "append" ((<>) @(Vector (Interpreter.Value Identity)))
  , Interpreter.builtIn "map" (Vector.map @(Interpreter.Value Identity) @(Interpreter.Value Identity))
      
  -- Strings
  , Interpreter.builtIn "appendString" ((<>) @Text)
      
  -- Booleans
  , Interpreter.builtIn "not" not
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
          let f :: JSON Aeson.Value -> JSON Aeson.Value
              f = runIdentity (Interpreter.interpret initialEnv m)
          BL8.putStrLn (Pretty.encodePretty (getJSON (f (JSON input))))
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This example implements a simple JSON processor, which
-- takes its instructions in the form of a PureScript program.
-- 
-- It receives its JSON input on stdin, and takes a filename
-- as its only command line argument. The file should contain
-- PureScript source for a module which defines a @main@
-- function for the query expression
--
-- It can be run on the command line as follows:
-- 
-- @
--   echo '\input -> input.foo' > query.purs
--   echo '{ "foo": 42, "bar": "baz" }' | query-json query.purs 
-- @
--
-- which should return output @42@.

module Main where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text.IO qualified as Text
import Language.PureScript.Interpreter (Eval, runEval)
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Make.Simplified qualified as Make
import Language.PureScript.Interpreter.JSON (JSON(..))
import Language.PureScript.Interpreter.Prelude (prelude)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (stdin)
      
main :: IO ()
main = do
  -- Read the module filename from the CLI arguments, and read the module source
  [moduleFile] <- getArgs
  moduleText <- Text.readFile moduleFile
  
  -- This helper function assists in writing the following code in a more 
  -- direct style:
  let orDie e f = either (die . f) pure e
  
  -- Compile the PureScript CoreFn output for the module
  buildResult <- Interpreter.runWithFFI [prelude] moduleFile moduleText
  build <- buildResult `orDie` Make.renderBuildError
  
  -- Read and parse the input JSON from standard input
  stdinBytes <- BL.hGetContents stdin
  input <- Aeson.eitherDecode stdinBytes `orDie` id
  
  -- Interpret the main function of the PureScript module as a Haskell function
  -- from JSON to JSON:
  let query :: JSON Aeson.Value -> Eval (JSON Aeson.Value)
      query = build
  
  -- Evaluate that function, then render the output as pretty-printed JSON on
  -- standard output.
  output <- runEval (query (JSON input)) `orDie` Interpreter.renderEvaluationError
  BL8.putStrLn (Pretty.encodePretty (getJSON output))
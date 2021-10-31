{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Interpreter
import Language.PureScript.Interpreter qualified as Interpreter
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
  
  -- Compile the PureScript CoreFn output for the module
  let buildResult :: Either InterpretError (JSON Aeson.Value -> Interpreter.Eval (JSON Aeson.Value))
      buildResult = runInterpret do
        ffi prelude
        CoreFn.Module{ CoreFn.moduleName } <- build moduleText
        evalMain moduleName
  
  -- This helper function assists in writing the following code in a more 
  -- direct style:
  let orDie e f = either (die . f) pure e
          
  -- Interpret the main function of the PureScript module as a Haskell function
  -- from JSON to JSON:
  query <- buildResult `orDie` renderInterpretError
  
  -- Read and parse the input JSON from standard input
  stdinBytes <- BL.hGetContents stdin
  input <- Aeson.eitherDecode stdinBytes `orDie` id
  
  -- Evaluate that function, then render the output as pretty-printed JSON on
  -- standard output.
  output <- Interpreter.runEval (query (JSON input)) 
              `orDie` Interpreter.renderEvaluationError
  BL8.putStrLn (Pretty.encodePretty (getJSON output))
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
--   cat > query.purs
--   module Main where
--   main input = input.foo
--   ^D
--
--   echo '{ "foo": 42, "bar": "baz" }' > input.json
--
--   query-json query.purs input.json
-- @
--
-- which should return output @42@.

module Main where

import Control.Monad.Trans.Class (lift)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Foldable (traverse_)
import Data.Text.IO qualified as Text
import Dovetail
import Dovetail.JSON (JSON(..))
import Dovetail.Prelude (stdlib)
import Language.PureScript.CoreFn qualified as CoreFn
import System.Environment (getArgs)
import System.Exit (die)
      
main :: IO ()
main = do
  -- Read the module filename from the CLI arguments, and read the module source
  [moduleFile, inputFile] <- getArgs
  moduleText <- Text.readFile moduleFile
        
  -- Read and parse the input JSON
  stdinBytes <- BL.readFile inputFile
  input <- either die pure (Aeson.eitherDecode stdinBytes)
  
  -- 'runInterpretTWithDebugger' will start a REPL debugger
  -- in the event of an error:
  runInterpretTWithDebugger $ do
    traverse_ ffi stdlib
    
    -- Compile the PureScript CoreFn output for the module:
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    
    -- Interpret the main function of the PureScript module as a Haskell function
    -- from JSON to JSON:
    query <- evalMain @_ @(JSON (Aeson.Value) -> EvalT IO (JSON Aeson.Value)) moduleName
    output <- liftEvalT (query (JSON input))
    
    -- Pretty-print the resulting JSON:
    lift $ BL8.putStrLn (Pretty.encodePretty (getJSON @Aeson.Value output))

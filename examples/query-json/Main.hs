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

import Control.Monad.Fix (MonadFix)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Interpreter (EvalT, Eval, runEval)
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Make.Simplified qualified as Make
import Language.PureScript.Interpreter.JSON (JSON(..))
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (stdin)

-- | The environment contains any Haskell functions which should be made
-- available to running PureScript code.
--
-- They are defined using 'Interpreter.builtIn' and given explicit type
-- signatures to improve type inference and error message quality.
--
-- These functions will be available in PureScript, with similar types, but
-- appearing pure (no mention of 'Interpreter.EvalT'). 
env :: forall m. MonadFix m => Interpreter.Env m
env = Map.unions
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

-- | Again, to aid comprehension, we provide a wrapper around the
-- 'Interpreter.run' function with an explicit type annotation.
--
-- The interpretation of our PureScript module is a function from JSON values
-- to JSON values. The interpreter will take care of any conversions to and
-- from PureScript values.
--
-- The 'Eval' monad must be used to wrap any result types, to track any
-- possible errors which might occur during evaluation.
run :: CoreFn.Module ann -> JSON Aeson.Value -> Eval (JSON Aeson.Value)
run = Interpreter.run env
      
main :: IO ()
main = do
  -- Read the module filename from the CLI arguments, and read the module source
  [moduleFile] <- getArgs
  moduleText <- Text.readFile moduleFile
  
  -- This helper function assists in writing the following code in a more 
  -- direct style:
  let orDie e f = either (die . f) pure e
  
  -- Compile the PureScript CoreFn output for the module
  buildResult <- Make.buildSingleModule moduleFile moduleText
  m <- buildResult `orDie` Make.renderBuildError
  
  -- Read and parse the input JSON from standard input
  stdinBytes <- BL.hGetContents stdin
  input <- Aeson.eitherDecode stdinBytes `orDie` id
  
  -- Interpret the main function of the PureScript module as a Haskell function
  -- from JSON to JSON:
  let query = run m
  
  -- Evaluate that function, then render the output as pretty-printed JSON on
  -- standard output.
  output <- runEval (query (JSON input)) `orDie` Interpreter.renderEvaluationError
  BL8.putStrLn (Pretty.encodePretty (getJSON output))
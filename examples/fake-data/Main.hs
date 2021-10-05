{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | This example implements a generator for randomized fake data, which
-- takes its instructions in the form of a PureScript program.
-- 
-- It takes a filename as its first command line argument, and a random
-- seed as an integer-valued second argument. The input file should contain
-- PureScript source for a module which defines a @main@
-- value for the expression which builds randomized outputs.
--
-- It can be run on the command line as follows:
-- 
-- @
--   echo '{ foo: choose [1, 2, 3] }' > build.purs
--   fake-data build.purs 12345
-- @

module Main where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (State, evalState, state)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text.IO qualified as Text
import Data.Vector (Vector, (!))
import Language.PureScript qualified as P
import Language.PureScript.Interpreter (EvalT, FFI(..), Value, runEvalT)
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Make.Simplified qualified as Make
import Language.PureScript.Interpreter.JSON (JSON(..))
import Language.PureScript.Interpreter.FFI ((-->))
import Language.PureScript.Interpreter.FFI qualified as FFI
import Language.PureScript.Interpreter.Prelude (prelude)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random qualified as Random

-- | For random number generation, we use a state monad whose state tracks a
-- standard deterministic generator.
type M = State Random.StdGen

-- | This example defines a single interesting function on the
-- Haskell side: the @choose@ function demonstrates the idea of using
-- side-effects in the interpreter - @choose@ chooses randomly between one of
-- its inputs, and returns the selected value
ffi :: FFI M
ffi = 
    FFI (P.ModuleName "Choose")
      [ ( P.Ident "choose"
        , FFI.forAll \a -> FFI.array a --> a
        , Interpreter.toValue choose
        )
      ]
  where
    choose :: Vector (Value M) -> EvalT M (Value M)
    choose xs = do
      idx <- lift (state (Random.randomR (0, length xs - 1)))
      pure (xs ! idx)

-- | Again, to aid comprehension, we provide a wrapper around the
-- 'id' function with an explicit type annotation.
--
-- The interpretation of our PureScript module is a JSON value, whose
-- computation may involve side-effects in the 'M' monad.
run :: EvalT M (JSON Aeson.Value) -> EvalT M (JSON Aeson.Value)
run = id
      
main :: IO ()
main = do
  -- Read the module filename from the CLI arguments, and read the module source
  [moduleFile, seedString] <- getArgs
  moduleText <- Text.readFile moduleFile
  let seed = read seedString :: Int
  
  -- This helper function assists in writing the following code in a more 
  -- direct style:
  let orDie e f = either (die . f) pure e
  
  -- Interpret the main function of the PureScript module as a non-deterministic
  -- JSON result
  buildResult <- Interpreter.runWithFFI [prelude] moduleFile moduleText
  build <- buildResult `orDie` Make.renderBuildError
  
  -- Create a deterministic random generator from the seed input
  let gen = Random.mkStdGen seed
  
  -- Evaluate that function, then render the output as pretty-printed JSON on
  -- standard output.
  output <- flip evalState gen (runExceptT (runEvalT (run build)))
              `orDie` Interpreter.renderEvaluationError
  BL8.putStrLn (Pretty.encodePretty (getJSON output))
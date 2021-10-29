{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
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

import Control.Monad.Fix (MonadFix)
import Control.Monad.State.Class (MonadState, state)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (evalStateT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text.IO qualified as Text
import Data.Vector (Vector, (!))
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Interpreter (EvalT, FFI(..), Value, runEvalT)
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Interpreter.FFIBuilder (array, (~>))
import Language.PureScript.Interpreter.FFIBuilder qualified as FFI
import Language.PureScript.Interpreter.JSON (JSON(..))
import Language.PureScript.Interpreter.Monad (InterpretError, build, evalMain, ffi, renderInterpretError, runInterpretT)
import Language.PureScript.Interpreter.Prelude (prelude)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random qualified as Random

-- | This example defines a single interesting function on the
-- Haskell side: the @choose@ function demonstrates the idea of using
-- side-effects in the interpreter - @choose@ chooses randomly between one of
-- its inputs, and returns the selected value
--
-- For random number generation, we use a state monad whose state tracks a
-- standard deterministic generator.
choose :: forall m. (MonadState Random.StdGen m, MonadFix m) => FFI m
choose = 
    FFI.evalFFIBuilder (P.ModuleName "Choose") do
      FFI.foreignImport (P.Ident "choose")
        (\a -> array a ~> a)
        choose_
  where
    choose_ :: Vector (Value m) -> EvalT m (Value m)
    choose_ xs = do
      idx <- lift (state (Random.randomR (0, length xs - 1)))
      pure (xs ! idx)
      
main :: IO ()
main = do
  -- Read the module filename from the CLI arguments, and read the module source
  [moduleFile, seedString] <- getArgs
  moduleText <- Text.readFile moduleFile
  
  -- The interpretation of our PureScript module is a JSON value, whose
  -- computation may involve side-effects in the 'M' monad.
  let buildResult 
        :: (MonadState Random.StdGen m, MonadFix m)
        => m (Either InterpretError (EvalT m (JSON Aeson.Value)))
      buildResult = runInterpretT do
        ffi prelude
        ffi choose
        CoreFn.Module{ CoreFn.moduleName } <- build moduleText
        evalMain moduleName
          
  let seed = read seedString :: Int
  
  -- This helper function assists in writing the following code in a more 
  -- direct style:
  let orDie e f = e >>= either (lift . die . f) pure
  
  -- Create a deterministic random generator from the seed input
  let gen = Random.mkStdGen seed
  
  flip evalStateT gen do
    -- Interpret the main function of the PureScript module as a non-deterministic
    -- JSON result
    value <- buildResult `orDie` renderInterpretError
  
    -- Evaluate that function, then render the output as pretty-printed JSON on
    -- standard output.
    output <- runEvalT value
                `orDie` Interpreter.renderEvaluationError
    lift (BL8.putStrLn (Pretty.encodePretty (getJSON output)))
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

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
--   cat > build.purs
--   module Main where
--   import Choose (choose)
--   main = { foo: choose [1, 2, 3] }
--   ^D
--
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
import Data.Foldable (traverse_)
import Data.Text.IO qualified as Text
import Data.Vector ((!))
import Dovetail
import Dovetail.Aeson qualified as JSON
import Dovetail.FFI.Builder qualified as FFI
import Dovetail.Prelude (stdlib)
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import System.Environment (getArgs)
import System.Exit (die)
import System.Random qualified as Random

main :: IO ()
main = do
  -- Read the module filename from the CLI arguments, and read the module source
  [moduleFile, seedString] <- getArgs
  moduleText <- Text.readFile moduleFile
  
  -- The interpretation of our PureScript module is a JSON value, whose
  -- computation may involve side-effects in the 'M' monad.
  let buildResult 
        :: (MonadState Random.StdGen m, MonadFix m)
        => m (Either (InterpretError m) Aeson.Value)
      buildResult = runInterpretT do
        traverse_ ffi stdlib
        
        _ <- JSON.stdlib
        
        -- This example defines a single interesting function on the
        -- Haskell side: the @choose@ function demonstrates the idea of using
        -- side-effects in the interpreter - @choose@ chooses randomly between one of
        -- its inputs, and returns the selected value
        --
        -- For random number generation, we use a state monad whose state tracks a
        -- standard deterministic generator.
        ffi $ FFI.evalFFIBuilder (P.ModuleName "Choose") do
          FFI.foreignImport (P.Ident "choose")
            (\a -> array a ~> a)
            \xs -> do
              idx <- lift (state (Random.randomR (0, length xs - 1)))
              pure (xs ! idx)
              
        CoreFn.Module{ CoreFn.moduleName } <- build moduleText
        
        -- Evaluate "main", returning JSON
        JSON.evalJSON (Just moduleName) "main"
          
  let seed = read seedString :: Int
  
  -- This helper function assists in writing the following code in a more 
  -- direct style:
  let orDie e f = e >>= either (lift . die . f) pure
  
  -- Create a deterministic random generator from the seed input
  let gen = Random.mkStdGen seed
  
  flip evalStateT gen do
    -- Interpret the main function of the PureScript module as a non-deterministic
    -- JSON result
    output <- buildResult `orDie` renderInterpretError defaultTerminalRenderValueOptions
  
    -- Render the output as pretty-printed JSON on standard output.
    lift (BL8.putStrLn (Pretty.encodePretty output))
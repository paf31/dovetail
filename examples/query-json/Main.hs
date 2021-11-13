{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
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

import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (lift)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Dovetail
import Dovetail.Aeson qualified as JSON 
import Dovetail.Evaluate qualified as Evaluate 
import Dovetail.Prelude (stdlib)
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import System.Environment (getArgs)
import System.Exit (die)
      
-- | This example uses the type of the user's program, which must be
-- a PureScript function of type @a -> b@, to synthesize an encoder for the
-- output data, and a decoder for the input data.
--
-- This function checks to make sure the program has a valid inferred type, and
-- synthesizes Haskell types @i@ and @o@ from the input and output types, 
-- so that they can be used for serialization.
checkTypeOfMain 
  :: MonadFix m
  => P.SourceType
  -> (forall i o. (JSON.Serializable m i, JSON.Serializable m o) => Proxy i -> Proxy o -> EvalT m r)
  -> EvalT m r
checkTypeOfMain (P.TypeApp _ (P.TypeApp _ fn inputTy) outputTy) f | fn == P.tyFunction =
  JSON.reify inputTy \inputProxy -> 
    JSON.reify outputTy \outputProxy -> 
      f inputProxy outputProxy
checkTypeOfMain (P.ForAll _ _ _ ty _) f =
  -- We simply strip off Forall constructors, since the JSON module will
  -- treat any occurrences of TypeVar as polymorphic data using UnknownJSON.
  checkTypeOfMain ty f
checkTypeOfMain _ _ =
  throwErrorWithContext (Evaluate.OtherError "main must have type a -> b where a and b are serializable")
      
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
    
    -- Include the JSON library, in case the user wants to handle or return
    -- nulls using 'JSON.Nullable'.
    _ <- JSON.stdlib
    
    -- Compile the PureScript CoreFn output for the module:
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    
    -- Interpret the main function of the PureScript module as a Haskell value
    -- (giving it the most general type 'Value', for now):
    (query, ty) <- eval (Just moduleName) "main"
    
    -- Next, check that @main@ is a function, and synthesize Haskell types for 
    -- the input and output based on its domain and codomain types respectively:
    output <- liftEvalT do
      checkTypeOfMain ty \(_ :: Proxy input) (_ :: Proxy output) ->
        -- Using the synthesized input type, decode the input:
        case Aeson.fromJSON @input input of
          Aeson.Success a -> do
            -- Now, attempt to coerce the user's program to the correct function
            -- type and evaluate the output:
            output <- fromValueRHS @IO @(input -> EvalT IO output) query a
            -- Finally, using the synthesized output type, encode the output as JSON:
            pure (Aeson.toJSON @output output)
          Aeson.Error err -> 
            throwErrorWithContext (Evaluate.OtherError ("error decoding input JSON: " <> Text.pack err))
              
    -- Pretty-print the resulting JSON:
    lift $ BL8.putStrLn (Pretty.encodePretty output)

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

import Debug.Trace
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
      
checkTypeOfMain :: P.SourceType -> Maybe (P.SourceType, P.SourceType)
checkTypeOfMain (P.TypeApp _ (P.TypeApp _ fn inputTy) outputTy)
  | fn == P.tyFunction
  = Just (inputTy, outputTy)
checkTypeOfMain (P.ForAll _ _ _ ty _) = 
  checkTypeOfMain ty
checkTypeOfMain _ =
  Nothing
      
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
    
    _ <- JSON.stdlib
    
    -- Compile the PureScript CoreFn output for the module:
    CoreFn.Module{ CoreFn.moduleName } <- build moduleText
    
    -- Interpret the main function of the PureScript module as a Haskell function
    -- from JSON to JSON:
    (query, ty) <- eval (Just moduleName) "main :: _ -> _"
    output <- liftEvalT do
      case checkTypeOfMain ty of
        Just (inputTy, outputTy) ->
          JSON.tryReifySerializableType inputTy \(_ :: Proxy input) -> 
            JSON.tryReifySerializableType outputTy \(_ :: Proxy output) -> 
              case Aeson.fromJSON @input input of
                Aeson.Success a -> do
                  Aeson.toJSON <$> fromValueRHS @IO @(input -> EvalT IO output) query a
                Aeson.Error err -> 
                  throwErrorWithContext (Evaluate.OtherError ("error decoding input JSON: " <> Text.pack err))
        _ -> 
          traceShow ty $ throwErrorWithContext (Evaluate.OtherError "main must have type a -> b where a and b are serializable")
    
    -- Pretty-print the resulting JSON:
    lift $ BL8.putStrLn (Pretty.encodePretty output)

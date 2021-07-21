{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT, evalContT, shiftT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Functor (void)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Version (Version)
import Interpreter qualified
import Interpreter.JSON (JSON(..), fromJSON, toJSON)
import Language.PureScript.AST.Declarations qualified as AST
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Make qualified as Make
import Language.PureScript.Names qualified as Names
import Language.PureScript.Options qualified as Options
import Language.PureScript.Sugar.Names.Env qualified as Env
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stdin)

initialEnv :: Interpreter.Env
initialEnv = Map.unions
  [ 
  -- Arrays
    Interpreter.builtIn "append" ((<>) @(Vector Interpreter.Value))
  , Interpreter.builtIn "map" $ \f xs ->
      Interpreter.Array <$> traverse (Interpreter.apply f) xs
      
  -- Strings
  , Interpreter.builtIn "appendString" ((<>) @Text)
      
  -- Booleans
  , Interpreter.builtIn "not" not
  ]
  
makeActions 
  :: (CoreFn.Module CoreFn.Ann -> IO ())
  -> Make.MakeActions Make.Make
makeActions k = Make.MakeActions
  { Make.getInputTimestampsAndHashes = \_ -> pure (Left Make.RebuildAlways)
  , Make.getOutputTimestamp = \_ -> pure Nothing
  , Make.readExterns = \_ -> error "readExterns: not supported"
  , Make.outputPrimDocs = error "outputPrimDocs: not supported"
  , Make.codegen = \m _ _ -> lift (liftIO (k m))
  , Make.ffiCodegen = \_ -> pure mempty
  , Make.progress = \_ -> pure mempty
  , Make.readCacheDb = pure mempty
  , Make.writeCacheDb = \_ -> pure mempty
  }
      
main :: IO ()
main = do
  [_, moduleFile] <- getArgs
  moduleText <- Text.readFile moduleFile
  case CST.parseFromFile moduleFile moduleText of
    (_, Left errs) ->
      putStrLn (show errs) *> exitFailure
    (_, Right m) 
      | AST.getModuleName m == Names.ModuleName "Main" -> evalContT $ do
          m <- shiftT $ \k -> lift . void $ do
            (result, _) <- Make.runMake Options.defaultOptions $
              Make.rebuildModule' (makeActions k) Env.primEnv [] m
            case result of
              Left errs ->
                putStrLn (show errs) *> exitFailure
              Right{} -> pure ()
          lift $ do
            stdinBytes <- BL.hGetContents stdin
            case Aeson.eitherDecode stdinBytes of
              Left err -> putStrLn err *> exitFailure
              Right (input :: Aeson.Value) -> do
                case Interpreter.interpretModule initialEnv m of
                  Left err -> putStrLn err *> exitFailure
                  Right f -> 
                    case Interpreter.apply f (fromJSON input) >>= Interpreter.fromValue of
                      Left err -> putStrLn err *> exitFailure
                      Right (result :: JSON Aeson.Value) -> 
                        BL8.putStrLn (Pretty.encodePretty (getJSON result))
      | otherwise ->
          putStrLn "Expected module name 'Main'" *> exitFailure
      
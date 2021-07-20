{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont qualified as Cont
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Version (Version)
import Data.Vector (Vector)
import Interpreter qualified
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.Names qualified as Names
import System.Exit (exitFailure)
import System.IO (stdin)
import System.Environment (getArgs)

newtype ParsedCoreFn = ParsedCoreFn { getParsedCoreFn :: (Version, CoreFn.Module CoreFn.Ann) }

instance Aeson.FromJSON ParsedCoreFn where
  parseJSON v = ParsedCoreFn <$> moduleFromJSON v

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
      
  -- Continuations
  , Interpreter.builtIn "callcc" $ \f ->
      Cont.callCC $ \k -> Interpreter.apply f (Interpreter.unsafeClosure (Names.Ident "cc") k)
  , Interpreter.builtIn "reset" $ \f ->
      Cont.resetT (Interpreter.apply f (Interpreter.Object mempty))
  , Interpreter.builtIn "shift" $ \f ->
      Cont.shiftT $ \k -> Interpreter.apply f (Interpreter.unsafeClosure @Interpreter.Value (Names.Ident "cc") (lift . k))
  ]

main :: IO ()
main = do
  [_, coreFnFile] <- getArgs
  coreFnBytes <- BL.readFile coreFnFile
  case Aeson.eitherDecode coreFnBytes of
    Left err -> putStrLn err *> exitFailure
    Right (ParsedCoreFn (ver, pursMod)) -> do
      stdinBytes <- BL.hGetContents stdin
      case Aeson.eitherDecode stdinBytes of
        Left err -> putStrLn err *> exitFailure
        Right (input :: Aeson.Value) -> do
          case Interpreter.interpretModule initialEnv pursMod input of
            Left err -> putStrLn err *> exitFailure
            Right (result :: Aeson.Value) -> BL8.putStrLn (Pretty.encodePretty result)
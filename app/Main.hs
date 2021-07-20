{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Supply (evalSupply)
import Control.Monad.Supply.Class (MonadSupply)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Version (Version)
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

initialEnv :: MonadSupply m => m Interpreter.Env
initialEnv = do
  _map <- Interpreter.toValue
  _append <- Interpreter.toValue
  pure $ Map.fromList
    [ ( Names.mkQualified (Names.Ident "map") (Names.ModuleName "Main")
      , _map $ \f xs ->
          Interpreter.Array <$> traverse (Interpreter.apply f) xs
      )
    , ( Names.mkQualified (Names.Ident "append") (Names.ModuleName "Main")
      , _append $ \xs ys -> 
          Interpreter.Array (xs <> ys)
      )
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
          case Interpreter.interpretModule (evalSupply 0 initialEnv) pursMod input of
            Left err -> putStrLn err *> exitFailure
            Right (result :: Aeson.Value) -> BL8.putStrLn (Pretty.encodePretty result)
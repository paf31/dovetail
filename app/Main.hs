{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as BL 
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Version (Version)
import Interpreter qualified
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import System.Exit (exitFailure)
import System.IO (stdin)
import System.Environment (getArgs)

newtype ParsedCoreFn = ParsedCoreFn { getParsedCoreFn :: (Version, CoreFn.Module CoreFn.Ann) }

instance Aeson.FromJSON ParsedCoreFn where
  parseJSON v = ParsedCoreFn <$> moduleFromJSON v

-- fromExpr :: Text -> Either String (CoreFn.Expr ())
-- fromExpr input = do
--     e <- parse input
-- 
--   where
--     parse input = do
--       let (_, e) = runParser (ParserState (Lexer.lex input) [] []) parseExpr
--       either (Left . show) Right e

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
          case Interpreter.interpretModule pursMod input of
            Left err -> putStrLn err *> exitFailure
            Right (result :: Aeson.Value) -> BL8.putStrLn (Pretty.encodePretty result)
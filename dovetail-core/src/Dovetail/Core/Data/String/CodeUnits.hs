{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.CodeUnits where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)


env :: forall m. MonadFix m => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

      _ModuleName = ModuleName "Data.String.CodeUnits"

  fold
    [ builtIn @m @(Char -> EvalT m Text)
        _ModuleName "singleton" 
        \c -> 
          pure (Text.singleton c)
    , builtIn @m @(Vector Char -> EvalT m Text)
        _ModuleName "fromCharArray" 
        \cs ->
          pure (Text.pack (Vector.toList cs))
    , builtIn @m @(Text -> EvalT m (Vector Char))
        _ModuleName "toCharArray" 
        \s -> 
          pure (Vector.fromList (Text.unpack s))
    , builtIn @m @((Char -> EvalT m (Value m)) -> Value m -> Text -> EvalT m (Value m)) 
        _ModuleName "_toChar" 
        \_just _nothing s ->
          case Text.uncons s of
            Just (c, s') | Text.null s' -> _just c
            _ -> pure _nothing
    , builtIn @m @((Char -> EvalT m (Value m)) -> Value m -> Integer -> Text -> EvalT m (Value m)) 
        _ModuleName "_charAt" 
        \_just _nothing i s ->
          if i >= 0 && i < fromIntegral (Text.length s)
            then _just (Text.index s (fromIntegral i))
            else pure _nothing
    , builtIn @m @(Text -> EvalT m Integer)
        _ModuleName "length" 
        \s ->
          pure (fromIntegral (Text.length s))
    , builtIn @m @((Char -> EvalT m Bool) -> Text -> EvalT m Integer)
        _ModuleName "countPrefix" 
        \p s ->
          notImplemented "countPrefix"
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Char -> Text -> EvalT m (Value m))  --  :: (Int -> Maybe Int) -> Maybe Int -> Char -> Int -> Text -> Maybe Int
        _ModuleName "_lastIndexOf" 
        \_just _nothing c s ->
          notImplemented "_lastIndexOf"
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Char -> Integer -> Text -> EvalT m (Value m))  --  :: (Int -> Maybe Int) -> Maybe Int -> Char -> Int -> Text -> Maybe Int
        _ModuleName "_indexOfStartingAt" 
        \_just _nothing c startAt s ->
          notImplemented "_indexOfStartingAt"
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Char -> Text -> EvalT m (Value m))  --  :: (Int -> Maybe Int) -> Maybe Int -> Char -> Int -> Text -> Maybe Int
        _ModuleName "_indexOf" 
        \_just _nothing c s ->
          notImplemented "_indexOf"
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Char -> Integer -> Text -> EvalT m (Value m))  --  :: (Int -> Maybe Int) -> Maybe Int -> Char -> Int -> Text -> Maybe Int
        _ModuleName "_lastIndexOfStartingAt" 
        \_just _nothing c startAt s ->
          notImplemented "_lastIndexOfStartingAt"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Int -> Text -> String
        _ModuleName "take" 
        \_ -> notImplemented "take"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Int -> Text -> String
        _ModuleName "drop" 
        \_ -> notImplemented "drop"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Int -> Int -> Text -> String
        _ModuleName "_slice" 
        \_ -> notImplemented "_slice"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Int -> Text -> { before :: String, after :: String }
        _ModuleName "splitAt" 
        \_ -> notImplemented "splitAt"
    ]
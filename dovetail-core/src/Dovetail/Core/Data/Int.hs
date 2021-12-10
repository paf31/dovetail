{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Int where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Char (chr, ord)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Dovetail
import Dovetail.Evaluate (builtIn)
import Numeric qualified

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Data.Int"

  fold
    [ -- fromNumberImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Number -> Maybe Int
      builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Double -> EvalT m (Value m))
        _ModuleName "fromNumberImpl" 
        \_just _nothing (properFraction -> (n, d)) ->
          if d == 0.0 then _just n else pure _nothing
      -- toNumber :: Int -> Number
    , builtIn @m @(Integer -> EvalT m Double)
        _ModuleName "toNumber"
        \a -> 
          pure (fromIntegral a)
      -- quot :: Int -> Int -> Int
    , builtIn @m @(Integer -> Integer -> EvalT m Integer)
        _ModuleName "quot"
        \a b -> 
          pure (a `quot` b)
      -- rem :: Int -> Int -> Int
    , builtIn @m @(Integer -> Integer -> EvalT m Integer)
        _ModuleName "rem"
        \a b -> 
          pure (a `rem` b)
      -- pow :: Int -> Int -> Int
    , builtIn @m @(Integer -> Integer -> EvalT m Integer)
        _ModuleName "pow"
        \a b -> 
          pure (a ^ b)
      -- fromStringAsImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Radix -> String -> Maybe Int
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Integer -> Text -> EvalT m (Value m))
        _ModuleName "fromStringAsImpl"
        \_just _nothing r s -> do
          when (r < 2 || r > 36) do
            throwErrorWithContext (OtherError "fromStringAsImpl: radix must be between 2 and 36")
          let readDigit :: Char -> Int
              readDigit c 
                | c >= '0' && c <= '9' = ord c - ord '0'
                | c >= 'a' && c <= 'z' = ord c - ord 'a' + 10
                | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 10
                | otherwise = error "readDigit: invalid input"
                
              isValidDigit :: Char -> Bool
              isValidDigit c
                | c >= '0' && c <= '9' = True
                | c >= 'a' && c <= 'z' = True
                | c >= 'A' && c <= 'Z' = True
                | otherwise = False
          case Numeric.readInt r isValidDigit readDigit (Text.unpack s) of
            [(x, "")] -> _just x
            _ -> pure _nothing
      -- toStringAs :: Radix -> Int -> String
    , builtIn @m @(Integer -> Integer -> EvalT m Text)
        _ModuleName "toStringAs"
        \r i -> do
          when (r < 2 || r > 36) do
            throwErrorWithContext (OtherError "toStringAs: radix must be between 2 and 36")
          let showDigit :: Int -> Char
              showDigit n 
                | n >= 0 && n < 10 = chr (ord '0' + n)
                | n >= 10 && n < 36 = chr (ord 'a' + n - 10)
                | otherwise = error "showDigit: invalid input"
          pure . Text.pack $ Numeric.showIntAtBase r showDigit i ""
    ]

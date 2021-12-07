{-# LANGUAGE BangPatterns        #-}
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
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.String.CodeUnits"

  let indexOf :: Text -> Text -> Maybe Int
      indexOf p s = 
        case Text.breakOn p s of
          (_, "") -> Nothing
          (prefix, _) -> Just (Text.length prefix)

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
          let loop n [] = pure n
              loop !n (c : cs) = do
                b <- p c
                if b then loop (n + 1) cs
                     else pure n
           in loop 0 (Text.unpack s)
      -- _lastIndexOf :: (Int -> Maybe Int) -> Maybe Int -> Char -> Int -> Text -> Maybe Int
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Text -> Text -> EvalT m (Value m))
        _ModuleName "_lastIndexOf" 
        \_just _nothing p s ->
          let len = Text.length s
           in maybe (pure _nothing) (_just . fromIntegral)
                (((len - 1) -) <$> indexOf p (Text.reverse s))
      -- _indexOfStartingAt :: (Int -> Maybe Int) -> Maybe Int -> Pattern -> Int -> Text -> Maybe Int
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Text -> Integer -> Text -> EvalT m (Value m))
        _ModuleName "_indexOfStartingAt" 
        \_just _nothing p (fromIntegral -> startAt) s ->
          maybe (pure _nothing) (_just . fromIntegral)
            ((startAt +) <$> indexOf p (Text.drop startAt s))
      -- _indexOf :: (Int -> Maybe Int) -> Maybe Int -> Pattern -> Int -> Text -> Maybe Int
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Text -> Text -> EvalT m (Value m))
        _ModuleName "_indexOf" 
        \_just _nothing p s ->
          maybe (pure _nothing) (_just . fromIntegral)
            (indexOf p s)
      -- _lastIndexOfStartingAt :: (Int -> Maybe Int) -> Maybe Int -> Pattern -> Int -> Text -> Maybe Int
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Text -> Integer -> Text -> EvalT m (Value m))
        _ModuleName "_lastIndexOfStartingAt" 
        \_just _nothing p (fromIntegral -> startAt) s ->
          maybe (pure _nothing) (_just . fromIntegral)
            (((startAt + Text.length p - 1) -) <$> 
              indexOf p (Text.reverse (Text.take (startAt + Text.length p) s)))
      -- take :: Int -> String -> String
    , builtIn @m @(Integer -> Text -> EvalT m Text)
        _ModuleName "take" 
        \n s -> 
          pure (Text.take (fromIntegral n) s)
      -- drop :: Int -> String -> String
    , builtIn @m @(Integer -> Text -> EvalT m Text)
        _ModuleName "drop" 
        \n s ->
          pure (Text.drop (fromIntegral n) s)
      -- _slice :: Int -> Int -> String -> String
    , builtIn @m @(Integer -> Integer -> Text -> EvalT m Text)
        _ModuleName "_slice" 
        \from to s -> 
          pure (Text.take (fromIntegral to - fromIntegral from) (Text.drop (fromIntegral from) s))
      -- splitAt :: Int -> Text -> { before :: String, after :: String }
    , builtIn @m @(Integer -> Text -> EvalT m (HashMap Text Text))
        _ModuleName "splitAt" 
        \i s -> 
          case Text.splitAt (fromIntegral i) s of
            (before, after) ->
              pure $ HashMap.fromList [("before", before), ("after", after)]
    ]

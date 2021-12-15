{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.CodeUnits where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.String.CodeUnits"

  let indexOf :: Text -> Text -> Maybe Int
      indexOf p s = 
        case Text.breakOn p s of
          (_, "") -> Nothing
          (prefix, _) -> Just (Text.length prefix)

  fold
    [ builtIn @ctx @(Char -> Eval ctx Text)
        _ModuleName "singleton" 
        \c -> 
          pure (Text.singleton c)
    , builtIn @ctx @(Vector Char -> Eval ctx Text)
        _ModuleName "fromCharArray" 
        \cs ->
          pure (Text.pack (Vector.toList cs))
    , builtIn @ctx @(Text -> Eval ctx (Vector Char))
        _ModuleName "toCharArray" 
        \s -> 
          pure (Vector.fromList (Text.unpack s))
    , builtIn @ctx @((Char -> Eval ctx (Value ctx)) -> Value ctx -> Text -> Eval ctx (Value ctx)) 
        _ModuleName "_toChar" 
        \_just _nothing s ->
          case Text.uncons s of
            Just (c, s') | Text.null s' -> _just c
            _ -> pure _nothing
    , builtIn @ctx @((Char -> Eval ctx (Value ctx)) -> Value ctx -> Integer -> Text -> Eval ctx (Value ctx)) 
        _ModuleName "_charAt" 
        \_just _nothing i s ->
          if i >= 0 && i < fromIntegral (Text.length s)
            then _just (Text.index s (fromIntegral i))
            else pure _nothing
    , builtIn @ctx @(Text -> Eval ctx Integer)
        _ModuleName "length" 
        \s ->
          pure (fromIntegral (Text.length s))
    , builtIn @ctx @((Char -> Eval ctx Bool) -> Text -> Eval ctx Integer)
        _ModuleName "countPrefix" 
        \p s ->
          let loop n [] = pure n
              loop !n (c : cs) = do
                b <- p c
                if b then loop (n + 1) cs
                     else pure n
           in loop 0 (Text.unpack s)
      -- _lastIndexOf :: (Int -> Maybe Int) -> Maybe Int -> Char -> Int -> Text -> Maybe Int
    , builtIn @ctx @((Integer -> Eval ctx (Value ctx)) -> Value ctx -> Text -> Text -> Eval ctx (Value ctx))
        _ModuleName "_lastIndexOf" 
        \_just _nothing p s ->
          let len = Text.length s
           in maybe (pure _nothing) (_just . fromIntegral)
                (((len - 1) -) <$> indexOf p (Text.reverse s))
      -- _indexOfStartingAt :: (Int -> Maybe Int) -> Maybe Int -> Pattern -> Int -> Text -> Maybe Int
    , builtIn @ctx @((Integer -> Eval ctx (Value ctx)) -> Value ctx -> Text -> Integer -> Text -> Eval ctx (Value ctx))
        _ModuleName "_indexOfStartingAt" 
        \_just _nothing p (fromIntegral -> startAt) s ->
          maybe (pure _nothing) (_just . fromIntegral)
            ((startAt +) <$> indexOf p (Text.drop startAt s))
      -- _indexOf :: (Int -> Maybe Int) -> Maybe Int -> Pattern -> Int -> Text -> Maybe Int
    , builtIn @ctx @((Integer -> Eval ctx (Value ctx)) -> Value ctx -> Text -> Text -> Eval ctx (Value ctx))
        _ModuleName "_indexOf" 
        \_just _nothing p s ->
          maybe (pure _nothing) (_just . fromIntegral)
            (indexOf p s)
      -- _lastIndexOfStartingAt :: (Int -> Maybe Int) -> Maybe Int -> Pattern -> Int -> Text -> Maybe Int
    , builtIn @ctx @((Integer -> Eval ctx (Value ctx)) -> Value ctx -> Text -> Integer -> Text -> Eval ctx (Value ctx))
        _ModuleName "_lastIndexOfStartingAt" 
        \_just _nothing p (fromIntegral -> startAt) s ->
          maybe (pure _nothing) (_just . fromIntegral)
            (((startAt + Text.length p - 1) -) <$> 
              indexOf p (Text.reverse (Text.take (startAt + Text.length p) s)))
      -- take :: Int -> String -> String
    , builtIn @ctx @(Integer -> Text -> Eval ctx Text)
        _ModuleName "take" 
        \n s -> 
          pure (Text.take (fromIntegral n) s)
      -- drop :: Int -> String -> String
    , builtIn @ctx @(Integer -> Text -> Eval ctx Text)
        _ModuleName "drop" 
        \n s ->
          pure (Text.drop (fromIntegral n) s)
      -- _slice :: Int -> Int -> String -> String
    , builtIn @ctx @(Integer -> Integer -> Text -> Eval ctx Text)
        _ModuleName "_slice" 
        \from to s -> 
          pure (Text.take (fromIntegral to - fromIntegral from) (Text.drop (fromIntegral from) s))
      -- splitAt :: Int -> Text -> { before :: String, after :: String }
    , builtIn @ctx @(Integer -> Text -> Eval ctx (HashMap Text Text))
        _ModuleName "splitAt" 
        \i s -> 
          case Text.splitAt (fromIntegral i) s of
            (before, after) ->
              pure $ HashMap.fromList [("before", before), ("after", after)]
    ]

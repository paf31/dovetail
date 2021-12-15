{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.CodePoints where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

type CodePoint = Integer

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.String.CodePoints"

  fold
    [ -- _singleton
      --   :: (CodePoint -> String)
      --   -> CodePoint
      --   -> String
      builtIn @ctx @((CodePoint -> Eval ctx Text) -> CodePoint -> Eval ctx Text)
        _ModuleName "_singleton" 
        \fallback -> fallback
      -- _fromCodePointArray
      --   :: (CodePoint -> String)
      --   -> Array CodePoint
      --   -> String
    , builtIn @ctx @((Vector CodePoint -> Eval ctx Text) -> Vector CodePoint -> Eval ctx Text)
        _ModuleName "_fromCodePointArray" 
        \fallback -> fallback
      -- _toCodePointArray
      --   :: (String -> Array CodePoint)
      --   -> (String -> CodePoint)
      --   -> String
      --   -> Array CodePoint
    , builtIn @ctx @((Text -> Eval ctx (Vector CodePoint)) -> (Text -> Eval ctx CodePoint) -> Text -> Eval ctx (Vector CodePoint))
        _ModuleName "_toCodePointArray" 
        \fallback _ -> fallback
      -- _codePointAt
      --   :: (Int -> String -> Maybe CodePoint)
      --   -> (forall a. a -> Maybe a)
      --   -> (forall a. Maybe a)
      --   -> (String -> CodePoint)
      --   -> Int
      --   -> String
      --   -> Maybe CodePoint
    , builtIn @ctx @((Integer -> Text -> Eval ctx (Value ctx)) -> (Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> (Text -> Eval ctx CodePoint) -> Integer -> Text -> Eval ctx (Value ctx))
        _ModuleName "_codePointAt" 
        \fallback _ _ _ -> fallback
      -- _countPrefix
      --   :: ((CodePoint -> Boolean) -> String -> Int)
      --   -> (String -> CodePoint)
      --   -> (CodePoint -> Boolean)
      --   -> String
      --   -> Int
    , builtIn @ctx @(((CodePoint -> Eval ctx Bool) -> Text -> Eval ctx Integer) -> (Text -> Eval ctx CodePoint) -> (CodePoint -> Eval ctx Bool) -> Text -> Eval ctx Integer)
        _ModuleName "_countPrefix" 
        \fallback _ -> fallback
      -- _take
      --   :: (Int -> String -> String)
      --   -> Int 
      --   -> String 
      --   -> String
    , builtIn @ctx @((Integer -> Text -> Eval ctx Text) -> Integer -> Text -> Eval ctx Text)
        _ModuleName "_take" 
        \fallback -> fallback
      -- _unsafeCodePointAt0
      --   :: (String -> CodePoint)
      --   -> String
      --   -> CodePoint
    , builtIn @ctx @((Text -> Eval ctx CodePoint) -> Text -> Eval ctx CodePoint)
        _ModuleName "_unsafeCodePointAt0" 
        \fallback -> fallback
    ]

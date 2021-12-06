{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.CodePoints where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

type CodePoint = Integer

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.String.CodePoints"

  fold
    [ -- _singleton
      --   :: (CodePoint -> String)
      --   -> CodePoint
      --   -> String
      builtIn @m @((CodePoint -> EvalT m Text) -> CodePoint -> EvalT m Text)
        _ModuleName "_singleton" 
        \fallback -> fallback
      -- _fromCodePointArray
      --   :: (CodePoint -> String)
      --   -> Array CodePoint
      --   -> String
    , builtIn @m @((Vector CodePoint -> EvalT m Text) -> Vector CodePoint -> EvalT m Text)
        _ModuleName "_fromCodePointArray" 
        \fallback -> fallback
      -- _toCodePointArray
      --   :: (String -> Array CodePoint)
      --   -> (String -> CodePoint)
      --   -> String
      --   -> Array CodePoint
    , builtIn @m @((Text -> EvalT m (Vector CodePoint)) -> (Text -> EvalT m CodePoint) -> Text -> EvalT m (Vector CodePoint))
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
    , builtIn @m @((Integer -> Text -> EvalT m (Value m)) -> (Value m -> EvalT m (Value m)) -> Value m -> (Text -> EvalT m CodePoint) -> Integer -> Text -> EvalT m (Value m))
        _ModuleName "_codePointAt" 
        \fallback _ _ _ -> fallback
      -- _countPrefix
      --   :: ((CodePoint -> Boolean) -> String -> Int)
      --   -> (String -> CodePoint)
      --   -> (CodePoint -> Boolean)
      --   -> String
      --   -> Int
    , builtIn @m @(((CodePoint -> EvalT m Bool) -> Text -> EvalT m Integer) -> (Text -> EvalT m CodePoint) -> (CodePoint -> EvalT m Bool) -> Text -> EvalT m Integer)
        _ModuleName "_countPrefix" 
        \fallback _ -> fallback
      -- _take
      --   :: (Int -> String -> String)
      --   -> Int 
      --   -> String 
      --   -> String
    , builtIn @m @((Integer -> Text -> EvalT m Text) -> Integer -> Text -> EvalT m Text)
        _ModuleName "_take" 
        \fallback -> fallback
      -- _unsafeCodePointAt0
      --   :: (String -> CodePoint)
      --   -> String
      --   -> CodePoint
    , builtIn @m @((Text -> EvalT m CodePoint) -> Text -> EvalT m CodePoint)
        _ModuleName "_unsafeCodePointAt0" 
        \fallback -> fallback
    ]

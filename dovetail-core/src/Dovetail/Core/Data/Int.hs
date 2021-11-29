{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Int where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

      _ModuleName = P.ModuleName "Data.Int"

  fold
    [ -- fromNumberImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Number -> Maybe Int
      builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> Double -> EvalT m (Value m))
        _ModuleName "fromNumberImpl" 
        \_just _nothing (properFraction -> (n, d)) ->
          if d == 0.0 then _just n else pure _nothing
    ]

-- toNumber :: Int -> Number
-- 
-- quot :: Int -> Int -> Int
-- 
-- rem :: Int -> Int -> Int
-- 
-- pow :: Int -> Int -> Int
-- 
-- fromStringAsImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Radix -> String -> Maybe Int
-- 
-- toStringAs :: Radix -> Int -> String

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Number.Format where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)
import Numeric qualified

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.Number.Format"

  fold
    [ -- toPrecisionNative :: Int -> Number -> String
      builtIn @m @(Integer -> Double -> EvalT m Text)
        _ModuleName "toPrecisionNative"
        \_ _ ->
          throwErrorWithContext (OtherError "toPrecisionNative is not implemented")
      -- toFixedNative :: Int -> Number -> String
    , builtIn @m @(Integer -> Double -> EvalT m Text)
        _ModuleName "toFixedNative"
        \p x ->
          pure . Text.pack $ Numeric.showFFloat (Just (fromIntegral p)) x ""
      -- toExponentialNative :: Int -> Number -> String
    , builtIn @m @(Integer -> Double -> EvalT m Text)
        _ModuleName "toExponentialNative"
        \p x ->
          pure . Text.pack $ Numeric.showEFloat (Just (fromIntegral p)) x ""
      -- toString :: Number -> String
    , builtIn @m @(Double -> EvalT m Text)
        _ModuleName "toString"
        \x ->
          pure . Text.pack $ Numeric.showFloat x ""
    ]



{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Semiring where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.Semiring"

  fold
    [ -- intAdd :: Int -> Int -> Int
      builtIn @m @(Integer -> Integer -> EvalT m Integer)
        _ModuleName "intAdd"
        \a b -> pure (a + b)
      -- intMul :: Int -> Int -> Int
    , builtIn @m @(Integer -> Integer -> EvalT m Integer)
        _ModuleName "intMul"
        \a b -> pure (a * b)
      -- numAdd :: Number -> Number -> Number
    , builtIn @m @(Double -> Double -> EvalT m Double)
        _ModuleName "numAdd"
        \a b -> pure (a + b)
      -- numMul :: Number -> Number -> Number
    , builtIn @m @(Double -> Double -> EvalT m Double)
        _ModuleName "numMul"
        \a b -> pure (a * b)
    ]
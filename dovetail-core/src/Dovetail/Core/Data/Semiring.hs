{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Semiring where

import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Semiring"

  fold
    [ -- intAdd :: Int -> Int -> Int
      builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "intAdd"
        \a b -> pure (a + b)
      -- intMul :: Int -> Int -> Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "intMul"
        \a b -> pure (a * b)
      -- numAdd :: Number -> Number -> Number
    , builtIn @ctx @(Double -> Double -> Eval ctx Double)
        _ModuleName "numAdd"
        \a b -> pure (a + b)
      -- numMul :: Number -> Number -> Number
    , builtIn @ctx @(Double -> Double -> Eval ctx Double)
        _ModuleName "numMul"
        \a b -> pure (a * b)
    ]
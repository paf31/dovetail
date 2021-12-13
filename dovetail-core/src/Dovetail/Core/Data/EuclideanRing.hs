{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.EuclideanRing where

import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.EuclideanRing"

  fold
    [ -- intDegree :: Int -> Int
      builtIn @ctx @(Integer -> Eval ctx Integer)
        _ModuleName "intDegree"
        \n -> 
          pure (abs n)
      -- intDiv :: Int -> Int -> Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "intDiv"
        \x y -> 
          pure (x `div` y)
      -- intMod :: Int -> Int -> Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "intMod"
        \x y ->
          pure (x `mod` y)
      -- numDiv :: Number -> Number -> Number
    , builtIn @ctx @(Double -> Double -> Eval ctx Double)
        _ModuleName "numDiv"
        \x y ->
          pure (x / y)
    ]
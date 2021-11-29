{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.EuclideanRing where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)


env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.EuclideanRing"

  fold
    [ -- intDegree :: Int -> Int
      builtIn @m @(Integer -> EvalT m Integer)
        _ModuleName "intDegree"
        \n -> 
          pure (abs n)
      -- intDiv :: Int -> Int -> Int
    , builtIn @m @(Integer -> Integer -> EvalT m Integer)
        _ModuleName "intDiv"
        \x y -> 
          pure (x `div` y)
      -- intMod :: Int -> Int -> Int
    , builtIn @m @(Integer -> Integer -> EvalT m Integer)
        _ModuleName "intMod"
        \x y ->
          pure (x `mod` y)
      -- numDiv :: Number -> Number -> Number
    , builtIn @m @(Double -> Double -> EvalT m Double)
        _ModuleName "numDiv"
        \x y ->
          pure (x / y)
    ]
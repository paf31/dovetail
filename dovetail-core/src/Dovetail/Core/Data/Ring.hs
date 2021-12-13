{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Ring where

import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Ring"

  fold
    [ -- intSub :: Int -> Int -> Int
      builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "intSub" 
        \a b -> pure (a - b)
    , -- numSub :: Number -> Number -> Number
      builtIn @ctx @(Double -> Double -> Eval ctx Double)
        _ModuleName "numSub" 
        \a b -> pure (a - b)
    ]
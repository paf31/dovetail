{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Ring where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)


env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Data.Ring"

  fold
    [ -- intSub :: Int -> Int -> Int
      builtIn @m @(Integer -> Integer -> EvalT m Integer)
        _ModuleName "intSub" 
        \a b -> pure (a - b)
    , -- numSub :: Number -> Number -> Number
      builtIn @m @(Double -> Double -> EvalT m Double)
        _ModuleName "numSub" 
        \a b -> pure (a - b)
    ]
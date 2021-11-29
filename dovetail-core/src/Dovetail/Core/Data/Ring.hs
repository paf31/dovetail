{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Ring where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = P.ModuleName "Data.Ring"

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
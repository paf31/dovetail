{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.HeytingAlgebra where

import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.HeytingAlgebra"

  fold
    [ -- boolConj :: Boolean -> Boolean -> Boolean
      builtIn @ctx @(Bool -> Bool -> Eval ctx Bool) 
        _ModuleName "boolConj" 
        \b1 b2 -> pure (b1 && b2)
    , -- boolDisj :: Boolean -> Boolean -> Boolean
      builtIn @ctx @(Bool -> Bool -> Eval ctx Bool) 
        _ModuleName "boolDisj" 
        \b1 b2 -> pure (b1 || b2)
    , -- boolNot :: Boolean -> Boolean
      builtIn @ctx @(Bool -> Eval ctx Bool) 
        _ModuleName "boolNot"
        \b -> pure (not b)
    ]
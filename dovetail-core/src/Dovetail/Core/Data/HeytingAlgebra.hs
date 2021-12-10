{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.HeytingAlgebra where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Data.HeytingAlgebra"

  fold
    [ -- boolConj :: Boolean -> Boolean -> Boolean
      builtIn @m @(Bool -> Bool -> EvalT m Bool) 
        _ModuleName "boolConj" 
        \b1 b2 -> pure (b1 && b2)
    , -- boolDisj :: Boolean -> Boolean -> Boolean
      builtIn @m @(Bool -> Bool -> EvalT m Bool) 
        _ModuleName "boolDisj" 
        \b1 b2 -> pure (b1 || b2)
    , -- boolNot :: Boolean -> Boolean
      builtIn @m @(Bool -> EvalT m Bool) 
        _ModuleName "boolNot"
        \b -> pure (not b)
    ]
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Control.Apply where

import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Control.Apply"

  -- arrayApply :: forall a b. Array (a -> b) -> Array a -> Array b
  builtIn @ctx @(Vector (Value ctx -> Eval ctx (Value ctx)) -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
    _ModuleName "arrayApply"
    \fs xs -> sequence (fs <*> xs)
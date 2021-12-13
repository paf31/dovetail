{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Functor where

import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Functor"

  -- arrayMap :: forall a b. (a -> b) -> Array a -> Array b
  builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
    _ModuleName "arrayMap"
    traverse


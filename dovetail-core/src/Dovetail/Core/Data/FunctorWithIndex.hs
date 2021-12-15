{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.FunctorWithIndex where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.FunctorWithIndex"

  -- mapWithIndexArray :: forall i a b. (i -> a -> b) -> Array a -> Array b
  builtIn @ctx @((Integer -> Value ctx -> Eval ctx (Value ctx)) -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
    _ModuleName "mapWithIndexArray"
    \f -> 
      Vector.imapM (f . fromIntegral)

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Foldable where

import Data.Foldable (fold)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Foldable"

  fold
    [ -- foldrArray :: forall a b. (a -> b -> b) -> b -> Array a -> b
      builtIn @ctx @((Value ctx -> Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> Vector (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "foldrArray"
        \f b -> Vector.foldM (flip f) b . Vector.reverse
      -- foldlArray :: forall a b. (b -> a -> b) -> b -> Array a -> b
    , builtIn @ctx @((Value ctx -> Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> Vector (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "foldlArray"
        Vector.foldM 
    ]
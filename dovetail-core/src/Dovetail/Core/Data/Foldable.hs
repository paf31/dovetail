{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Foldable where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.Foldable"

  fold
    [ -- foldrArray :: forall a b. (a -> b -> b) -> b -> Array a -> b
      builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> Value m -> Vector (Value m) -> EvalT m (Value m))
        _ModuleName "foldrArray"
        \f b -> Vector.foldM (flip f) b . Vector.reverse
      -- foldlArray :: forall a b. (b -> a -> b) -> b -> Array a -> b
    , builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> Value m -> Vector (Value m) -> EvalT m (Value m))
        _ModuleName "foldlArray"
        Vector.foldM 
    ]
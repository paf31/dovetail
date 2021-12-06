{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Functor where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Traversable (traverse)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.Functor"

  -- arrayMap :: forall a b. (a -> b) -> Array a -> Array b
  builtIn @m @((Value m -> EvalT m (Value m)) -> Vector (Value m) -> EvalT m (Vector (Value m)))
    _ModuleName "arrayMap"
    traverse

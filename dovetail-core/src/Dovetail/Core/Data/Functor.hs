{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Functor where

import Control.Monad.IO.Class (MonadIO)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Data.Functor"

  -- arrayMap :: forall a b. (a -> b) -> Array a -> Array b
  builtIn @m @((Value m -> EvalT m (Value m)) -> Vector (Value m) -> EvalT m (Vector (Value m)))
    _ModuleName "arrayMap"
    traverse


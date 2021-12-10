{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.FunctorWithIndex where

import Control.Monad.IO.Class (MonadIO)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Data.FunctorWithIndex"

  -- mapWithIndexArray :: forall i a b. (i -> a -> b) -> Array a -> Array b
  builtIn @m @((Integer -> Value m -> EvalT m (Value m)) -> Vector (Value m) -> EvalT m (Vector (Value m)))
    _ModuleName "mapWithIndexArray"
    \f -> 
      Vector.imapM (f . fromIntegral)

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Control.Extend where

import Control.Monad.Fix (MonadFix)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Control.Extend"

  -- arrayExtend :: forall a b. (Array a -> b) -> Array a -> Array b
  builtIn @m @((Vector (Value m) -> EvalT m (Value m)) -> Vector (Value m) -> EvalT m (Vector (Value m)))
    _ModuleName "arrayExtend"
    \f xs ->
      Vector.generateM (Vector.length xs) \i ->
        f (Vector.drop i xs)
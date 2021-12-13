{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Control.Bind where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Control.Bind"

  -- arrayBind :: forall a b. Array a -> (a -> Array b) -> Array b
  builtIn @ctx @(Vector (Value ctx) -> (Value ctx -> Eval ctx (Vector (Value ctx))) -> Eval ctx (Vector (Value ctx)))
    _ModuleName "arrayBind"
    \xs f -> Vector.concat <$> traverse f (Vector.toList xs)
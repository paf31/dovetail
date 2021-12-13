{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Unsafe where

import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Effect.Unsafe"

  -- unsafePerformEffect :: forall a. Effect a -> a
  builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Eval ctx (Value ctx))
    _ModuleName "unsafePerformEffect"
    \f -> f (Object mempty)
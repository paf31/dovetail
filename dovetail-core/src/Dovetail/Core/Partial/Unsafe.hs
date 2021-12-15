{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Partial.Unsafe where

import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Partial.Unsafe"

  -- _unsafePartial :: forall a b. a -> b
  builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Eval ctx (Value ctx))
    _ModuleName "_unsafePartial"
    \f -> f (Object mempty)
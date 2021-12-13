{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Unsafe.Coerce where

import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Unsafe.Coerce"

  -- unsafeCoerce :: forall a b. a -> b
  builtIn @ctx @(Value ctx -> Eval ctx (Value ctx))
    _ModuleName "unsafeCoerce" 
    pure
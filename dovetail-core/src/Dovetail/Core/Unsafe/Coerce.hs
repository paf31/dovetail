{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Unsafe.Coerce where

import Control.Monad.Fix (MonadFix)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Unsafe.Coerce"

  -- unsafeCoerce :: forall a b. a -> b
  builtIn @m @(Value m -> EvalT m (Value m))
    _ModuleName "unsafeCoerce" 
    pure
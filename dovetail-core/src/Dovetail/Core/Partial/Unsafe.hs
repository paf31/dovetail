{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Partial.Unsafe where

import Control.Monad.Fix (MonadFix)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Partial.Unsafe"

  -- _unsafePartial :: forall a b. a -> b
  builtIn @m @((Value m -> EvalT m (Value m)) -> EvalT m (Value m))
    _ModuleName "_unsafePartial"
    \f -> f (Object mempty)
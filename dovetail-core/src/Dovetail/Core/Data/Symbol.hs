{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Symbol where

import Control.Monad.IO.Class (MonadIO)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Data.Symbol"

  -- unsafeCoerce :: forall a b. a -> b
  builtIn @m @(Value m -> EvalT m (Value m))
    _ModuleName "unsafeCoerce"
    pure
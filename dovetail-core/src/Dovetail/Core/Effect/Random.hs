{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Random where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Fix (MonadFix)
import Dovetail
import Dovetail.Evaluate (builtIn)
import System.Random (randomRIO)

env :: forall m. (MonadFix m, MonadIO m) => Env m
env = do
  let _ModuleName = ModuleName "Effect.Random"

  -- random :: Effect Number
  builtIn @m @(Value m -> EvalT m Double)
    _ModuleName "random"
    \_ ->
      liftIO (randomRIO (0.0, 1.0))

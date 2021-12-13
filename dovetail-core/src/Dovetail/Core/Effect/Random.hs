{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Random where

import Control.Monad.IO.Class (MonadIO(..))
import Dovetail
import Dovetail.Evaluate (builtIn)
import System.Random (randomRIO)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Effect.Random"

  -- random :: Effect Number
  builtIn @ctx @(Value ctx -> Eval ctx Double)
    _ModuleName "random"
    \_ ->
      liftIO (randomRIO (0.0, 1.0))

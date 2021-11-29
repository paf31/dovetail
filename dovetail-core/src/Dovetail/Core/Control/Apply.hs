{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Control.Apply where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (sequence)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = P.ModuleName "Control.Apply"

  fold
    [ -- arrayApply :: forall a b. Array (a -> b) -> Array a -> Array b
      builtIn @m @(Vector (Value m -> EvalT m (Value m)) -> Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "arrayApply"
        \fs xs -> sequence (fs <*> xs)
    ]
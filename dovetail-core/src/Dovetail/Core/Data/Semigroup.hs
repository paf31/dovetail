{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Semigroup where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.Semigroup"

  fold
    [ -- concatString :: String -> String -> String
      builtIn @m @(Text -> Text -> EvalT m Text)
        _ModuleName "concatString"
        \xs ys ->
          pure (xs <> ys)
      -- concatArray :: forall a. Array a -> Array a -> Array a
    , builtIn @m @(Vector (Value m) -> Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "concatArray"
        \xs ys ->
          pure (xs <> ys)
    ]
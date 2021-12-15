{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Semigroup where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Semigroup"

  fold
    [ -- concatString :: String -> String -> String
      builtIn @ctx @(Text -> Text -> Eval ctx Text)
        _ModuleName "concatString"
        \xs ys ->
          pure (xs <> ys)
      -- concatArray :: forall a. Array a -> Array a -> Array a
    , builtIn @ctx @(Vector (Value ctx) -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "concatArray"
        \xs ys ->
          pure (xs <> ys)
    ]
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Bounded where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.Bounded"

  fold
    [ -- topInt :: Int
      builtIn @m @Integer
        _ModuleName "topInt"
        2147483647
      -- bottomInt :: Int
    , builtIn @m @Integer
        _ModuleName "bottomInt"
        (-2147483648)
      -- topChar :: Char
    , builtIn @m @Char
        _ModuleName "topChar"
        (maxBound @Char)
      -- bottomChar :: Char
    , builtIn @m @Char
        _ModuleName "bottomChar"
        (minBound @Char)
      -- topNumber :: Number
    , builtIn @m @Double
        _ModuleName "topNumber"
        (recip 0)
      -- bottomNumber :: Number
    , builtIn @m @Double
        _ModuleName "bottomNumber"
        (-(recip 0))
    ]
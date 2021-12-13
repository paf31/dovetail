{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Bounded where

import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Bounded"

  fold
    [ -- topInt :: Int
      builtIn @ctx @Integer
        _ModuleName "topInt"
        2147483647
      -- bottomInt :: Int
    , builtIn @ctx @Integer
        _ModuleName "bottomInt"
        (-2147483648)
      -- topChar :: Char
    , builtIn @ctx @Char
        _ModuleName "topChar"
        (maxBound @Char)
      -- bottomChar :: Char
    , builtIn @ctx @Char
        _ModuleName "bottomChar"
        (minBound @Char)
      -- topNumber :: Number
    , builtIn @ctx @Double
        _ModuleName "topNumber"
        (recip 0)
      -- bottomNumber :: Number
    , builtIn @ctx @Double
        _ModuleName "bottomNumber"
        (-(recip 0))
    ]
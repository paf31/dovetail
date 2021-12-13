{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Number where

import Data.Foldable (fold)
import Data.Text (Text)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Number"

  fold
    [ -- nan :: Number
      builtIn @ctx @Double
        _ModuleName "nan"
        (0 / 0)
    -- isNaN :: Number -> Boolean
    , builtIn @ctx @(Double -> Eval ctx Bool)
        _ModuleName "isNaN"
        \a -> 
          pure (isNaN a)
    -- infinity :: Number
    , builtIn @ctx @Double
        _ModuleName "infinity"
        (recip 0)
    -- isFinite :: Number -> Boolean
    , builtIn @ctx @(Double -> Eval ctx Bool)
        _ModuleName "isFinite"
        \a -> 
          pure (not (isInfinite a))
    -- fromStringImpl :: Fn4 String (Number -> Boolean) (forall a. a -> Maybe a) (forall a. Maybe a) (Maybe Number)
    , builtIn @ctx @(Text -> (Double -> Eval ctx Bool) -> (Double -> Eval ctx (Value ctx)) -> Value ctx -> Eval ctx (Value ctx))
        _ModuleName "fromStringImpl"
        \_s _isFinite _just _nothing ->
          throwErrorWithContext (OtherError "fromStringImpl is not implemented")
    ]
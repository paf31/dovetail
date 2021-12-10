{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Number where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Data.Text (Text)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Data.Number"

  fold
    [ -- nan :: Number
      builtIn @m @Double
        _ModuleName "nan"
        (0 / 0)
    -- isNaN :: Number -> Boolean
    , builtIn @m @(Double -> EvalT m Bool)
        _ModuleName "isNaN"
        \a -> 
          pure (isNaN a)
    -- infinity :: Number
    , builtIn @m @Double
        _ModuleName "infinity"
        (recip 0)
    -- isFinite :: Number -> Boolean
    , builtIn @m @(Double -> EvalT m Bool)
        _ModuleName "isFinite"
        \a -> 
          pure (not (isInfinite a))
    -- fromStringImpl :: Fn4 String (Number -> Boolean) (forall a. a -> Maybe a) (forall a. Maybe a) (Maybe Number)
    , builtIn @m @(Text -> (Double -> EvalT m Bool) -> (Double -> EvalT m (Value m)) -> Value m -> EvalT m (Value m))
        _ModuleName "fromStringImpl"
        \_s _isFinite _just _nothing ->
          throwErrorWithContext (OtherError "fromStringImpl is not implemented")
    ]
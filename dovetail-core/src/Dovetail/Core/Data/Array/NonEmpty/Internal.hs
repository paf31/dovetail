{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Array.NonEmpty.Internal where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = P.ModuleName "Data.Array.NonEmpty.Internal"

  fold
    [ -- foldr1Impl :: forall a. (a -> a -> a) -> NonEmptyArray a -> a
      builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> Vector (Value m) -> EvalT m (Value m))
        _ModuleName "foldr1Impl"
        \f -> 
          Vector.fold1M (flip f) . Vector.reverse
      -- foldl1Impl :: forall a. (a -> a -> a) -> NonEmptyArray a -> a
    , builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> Vector (Value m) -> EvalT m (Value m))
        _ModuleName "foldl1Impl"
        Vector.fold1M
      -- traverse1Impl
      --  :: forall m a b
      --   . (forall a' b'. (m (a' -> b') -> m a' -> m b'))
      --  -> (forall a' b'. (a' -> b') -> m a' -> m b')
      --  -> (a -> m b)
      --  -> NonEmptyArray a
      --  -> m (NonEmptyArray b)
    , builtIn @m @(
           (Value m -> Value m -> EvalT m (Value m))
        -> ((Vector (Value m) -> Value m -> EvalT m (Vector (Value m))) -> Value m -> EvalT m (Value m))
        -> (Value m -> EvalT m (Value m))
        -> Vector (Value m)
        -> EvalT m (Value m)
        )
        _ModuleName "traverse1Impl"
        \_apply _fmap f xs -> do
          let _cons :: Value m -> Value m -> EvalT m (Value m)
              _cons bs a = do
                b <- f a
                _fmap (\ys y -> pure (Vector.snoc ys y)) bs >>= (`_apply` b)
          Vector.fold1M _cons xs
    ]
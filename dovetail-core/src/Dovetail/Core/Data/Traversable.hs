{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Traversable where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall ctx. Env ctx
env = do
  let _ModuleName = P.ModuleName "Data.Traversable"

  -- traverseArrayImpl
  --   :: forall m a b
  --    . (m (a -> b) -> m a -> m b)
  --   -> ((a -> b) -> m a -> m b)
  --   -> (a -> m a)
  --   -> (a -> m b)
  --   -> Array a
  --   -> m (Array b)
  builtIn @ctx @(
       (Value ctx -> Value ctx -> Eval ctx (Value ctx))
    -> ((Vector (Value ctx) -> Value ctx -> Eval ctx (Vector (Value ctx))) -> Value ctx -> Eval ctx (Value ctx))
    -> (Vector (Value ctx) -> Eval ctx (Value ctx))
    -> (Value ctx -> Eval ctx (Value ctx))
    -> Vector (Value ctx)
    -> Eval ctx (Value ctx)
    )
    _ModuleName "traverseArrayImpl"
    \_apply _fmap _pure f xs -> do
      bs0 <- _pure Vector.empty
      let _cons :: Value ctx -> Value ctx -> Eval ctx (Value ctx)
          _cons bs a = do
            b <- f a
            _fmap (\ys y -> pure (Vector.snoc ys y)) bs >>= (`_apply` b)
      Vector.foldM _cons bs0 xs
      
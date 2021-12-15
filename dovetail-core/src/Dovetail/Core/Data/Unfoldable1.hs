{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Unfoldable1 where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

-- unfoldrArrayImpl
--   :: forall a b
--    . (forall x. Maybe x -> Boolean)
--   -> (forall x. Maybe x -> x)
--   -> (forall x y. Tuple x y -> x)
--   -> (forall x y. Tuple x y -> y)
--   -> (b -> Tuple a (Maybe b))
--   -> b
--   -> Array a
unfoldr1ArrayImpl 
  :: (Value ctx -> Eval ctx Bool)
  -> (Value ctx -> Eval ctx (Value ctx))
  -> (Value ctx -> Eval ctx (Value ctx))
  -> (Value ctx -> Eval ctx (Value ctx))
  -> (Value ctx -> Eval ctx (Value ctx)) 
  -> Value ctx 
  -> Eval ctx (Vector (Value ctx))
unfoldr1ArrayImpl _isNothing _fromJust _fst _snd _f _x = do
  let toMaybe x = _isNothing x >>= \b -> if b then pure Nothing else Just <$> _fromJust x
      toTuple x = (,) <$> _fst x <*> _snd x
  (a, mb) <- toTuple =<< _f _x
  Vector.cons a <$> Vector.unfoldrM (\_mb -> do 
    mb' <- toMaybe _mb
    traverse ((>>= toTuple) . _f) mb') mb

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Unfoldable1"

  builtIn @ctx @_
    _ModuleName "unfoldr1ArrayImpl"
    unfoldr1ArrayImpl

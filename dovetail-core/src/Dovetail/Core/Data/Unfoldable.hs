{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Unfoldable where

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
--   -> (b -> Maybe (Tuple a b))
--   -> b
--   -> Array a
unfoldrArrayImpl 
  :: (Value ctx -> Eval ctx Bool)
  -> (Value ctx -> Eval ctx (Value ctx))
  -> (Value ctx -> Eval ctx (Value ctx))
  -> (Value ctx -> Eval ctx (Value ctx))
  -> (Value ctx -> Eval ctx (Value ctx)) 
  -> Value ctx 
  -> Eval ctx (Vector (Value ctx))
unfoldrArrayImpl _isNothing _fromJust _fst _snd _f _x = do
  let toMaybe x = _isNothing x >>= \b -> if b then pure Nothing else Just <$> _fromJust x
      toTuple x = (,) <$> _fst x <*> _snd x
  Vector.unfoldrM (\b -> do 
    x <- _f b 
    m <- toMaybe x
    traverse toTuple m) _x

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Unfoldable"

  builtIn @ctx @_
    _ModuleName "unfoldrArrayImpl"
    unfoldrArrayImpl

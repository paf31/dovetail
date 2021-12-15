{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Array where

import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as Mutable
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall ctx. Env ctx
env = do
  let _ModuleName = P.ModuleName "Data.Array"

  fold
    [ -- fromFoldableImpl
      --   :: forall f a
      --    . (forall b. (a -> b -> b) -> b -> f a -> b)
      --   -> f a
      --   -> Array a
      builtIn @ctx @(((Value ctx -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx))) -> Vector (Value ctx) -> Value ctx -> Eval ctx (Vector (Value ctx))) -> Value ctx -> Eval ctx (Vector (Value ctx)))
        _ModuleName "fromFoldableImpl"
        \_foldr xs ->
          _foldr (\y ys -> pure (Vector.cons y ys)) Vector.empty xs
      -- range :: Int -> Int -> Array Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx (Vector Integer))
        _ModuleName "range"
        \from to -> 
          pure (Vector.fromList [from..to])
      -- replicate :: forall a. Int -> a -> Array a
    , builtIn @ctx @(Integer -> Value ctx -> Eval ctx (Vector (Value ctx)))
        _ModuleName "replicate"
        \n a ->
          pure (Vector.replicate (fromIntegral n) a)
      -- length :: forall a. Array a -> Int
    , builtIn @ctx @(Vector (Value ctx) -> Eval ctx Integer)
        _ModuleName "length"
        \v ->
          pure (fromIntegral (Vector.length v))
      -- unconsImpl :: forall a b
      --    . (Unit -> b)
      --   -> (a -> Array a -> b)
      --   -> Array a
      --   -> b
    , builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> (Value ctx -> Vector (Value ctx) -> Eval ctx (Value ctx)) -> Vector (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "unconsImpl"
        \_nothing _just xs ->
          case xs Vector.!? 0 of
            Nothing -> _nothing (Object mempty)
            Just hd -> _just hd (Vector.tail xs)
      -- indexImpl :: forall a
      --    . (forall r. r -> Maybe r)
      --   -> (forall r. Maybe r)
      --   -> Array a
      --   -> Int
      --   -> Maybe a
    , builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> Vector (Value ctx) -> Integer -> Eval ctx (Value ctx))
        _ModuleName "indexImpl"
        \_just _nothing xs i ->
          case xs Vector.!? fromIntegral i of
            Nothing -> pure _nothing
            Just x -> _just x
      -- findMapImpl :: forall a b
      --    . (forall c. Maybe c)
      --   -> (forall c. Maybe c -> Boolean)
      --   -> (a -> Maybe b)
      --   -> Array a
      --   -> Maybe b
    , builtIn @ctx @(Value ctx -> (Value ctx -> Eval ctx Bool) -> (Value ctx -> Eval ctx (Value ctx)) -> Vector (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "findMapImpl"
        \_nothing _isJust _f _xs ->
          throwErrorWithContext (OtherError "findMapImpl is not implemented")
      -- findIndexImpl :: forall a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> (a -> Boolean)
      --   -> Array a
      --   -> Maybe Int
    , builtIn @ctx @((Integer -> Eval ctx (Value ctx)) -> Value ctx -> (Value ctx -> Eval ctx Bool) -> Vector (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "findIndexImpl"
        \_just _nothing _p _xs ->
          throwErrorWithContext (OtherError "findIndexImpl is not implemented")
      -- findLastIndexImpl :: forall a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> (a -> Boolean)
      --   -> Array a
      --   -> Maybe Int
    , builtIn @ctx @((Integer -> Eval ctx (Value ctx)) -> Value ctx -> (Value ctx -> Eval ctx Bool) -> Vector (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "findLastIndexImpl"
        \_just _nothing _p _xs ->
          throwErrorWithContext (OtherError "findLastIndexImpl is not implemented")
      -- _insertAt :: forall a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> Int
      --   -> a
      --   -> Array a
      --   -> Maybe (Array a)
    , builtIn @ctx @((Vector (Value ctx) -> Eval ctx (Value ctx)) -> Value ctx -> Integer -> Value ctx -> Vector (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "_insertAt"
        \_just _nothing i x xs ->
          if i >= 0 && fromIntegral i <= Vector.length xs
            then let (ys, zs) = Vector.splitAt (fromIntegral i) xs
                  in _just (ys <> pure x <> zs)
            else pure _nothing
      -- _deleteAt :: forall a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> Int
      --   -> Array a
      --   -> Maybe (Array a)
    , builtIn @ctx @((Vector (Value ctx) -> Eval ctx (Value ctx)) -> Value ctx -> Integer -> Vector (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "_deleteAt"
        \_just _nothing i xs ->
          if i >= 0 && fromIntegral i < Vector.length xs
            then let (ys, zs) = Vector.splitAt (fromIntegral i) xs
                  in _just (ys <> Vector.drop 1 zs)
            else pure _nothing
      -- _updateAt :: forall a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> Int
      --   -> a
      --   -> Array a
      --   -> Maybe (Array a)
    , builtIn @ctx @((Vector (Value ctx) -> Eval ctx (Value ctx)) -> Value ctx -> Integer -> Value ctx -> Vector (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "_updateAt"
        \_just _nothing i x xs ->
          if i >= 0 && fromIntegral i < Vector.length xs 
            then _just $ Vector.modify (\mut -> Mutable.write mut (fromIntegral i) x) xs
            else pure _nothing
      -- reverse :: forall a. Array a -> Array a
    , builtIn @ctx @(Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "reverse"
        \xs ->
          pure (Vector.reverse xs)
      -- concat :: forall a. Array (Array a) -> Array a
    , builtIn @ctx @(Vector (Vector (Value ctx)) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "concat"
        \xss ->
          pure (Vector.concat (Vector.toList xss))
      -- filter :: forall a. (a -> Boolean) -> Array a -> Array a
    , builtIn @ctx @((Value ctx -> Eval ctx Bool) -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "filter"
        \p xs ->
          Vector.filterM p xs
      -- partition :: forall a
      --    . (a -> Boolean)
      --   -> Array a
      --   -> { yes :: Array a, no :: Array a }
    , builtIn @ctx @((Value ctx -> Eval ctx Bool) -> Vector (Value ctx) -> Eval ctx (HashMap Text (Vector (Value ctx))))
        _ModuleName "partition"
        \p xs ->
          let mkV yes no = HashMap.fromList [("yes", yes), ("no", no)]
           in mkV <$> Vector.filterM p xs <*> Vector.filterM (fmap not . p) xs
      -- scanl :: forall a b. (b -> a -> b) -> b -> Array a -> Array b
    , builtIn @ctx @((Value ctx -> Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "scanl"
        \_f _b _xs ->
          throwErrorWithContext (OtherError "scanl is not implemented")
      -- scanr :: forall a b. (a -> b -> b) -> b -> Array a -> Array b
    , builtIn @ctx @((Value ctx -> Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "scanr"
        \_f _b _xs ->
          throwErrorWithContext (OtherError "scanr is not implemented")
      -- sortByImpl :: forall a. (a -> a -> Ordering) -> (Ordering -> Int) -> Array a -> Array a
    , builtIn @ctx @((Value ctx -> Value ctx -> Eval ctx (Value ctx)) -> (Value ctx -> Eval ctx Integer) -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "sortByImpl"
        \_f _g _xs ->
          throwErrorWithContext (OtherError "sortByImpl is not implemented")
      -- slice :: forall a. Int -> Int -> Array a -> Array a
    , builtIn @ctx @(Integer -> Integer -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "slice"
        \startAt len xs ->
          pure (Vector.slice (fromIntegral startAt) (fromIntegral len) xs)
      -- zipWith :: forall a b c
      --    . (a -> b -> c)
      --   -> Array a
      --   -> Array b
      --   -> Array c
    , builtIn @ctx @((Value ctx -> Value ctx -> Eval ctx (Value ctx)) -> Vector (Value ctx) -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "zipWith"
        \f xs ys ->
          Vector.zipWithM f xs ys
      -- any :: forall a. (a -> Boolean) -> Array a -> Boolean
    , builtIn @ctx @((Value ctx -> Eval ctx Bool) -> Vector (Value ctx) -> Eval ctx Bool)
        _ModuleName "any"
        \f xs ->
          Vector.or <$> traverse f xs
      -- all :: forall a. (a -> Boolean) -> Array a -> Boolean
      , builtIn @ctx @((Value ctx -> Eval ctx Bool) -> Vector (Value ctx) -> Eval ctx Bool)
        _ModuleName "all"
        \f xs ->
          Vector.and <$> traverse f xs
      -- unsafeIndexImpl :: forall a. Array a -> Int -> a
    , builtIn @ctx @(Vector (Value ctx) -> Integer -> Eval ctx (Value ctx))
        _ModuleName "unsafeIndexImpl"
        \xs i ->
          case xs Vector.!? fromIntegral i of
            Nothing -> throwErrorWithContext (OtherError "unsafeIndexImpl: index out of range")
            Just a -> pure a
    ]
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Array where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as Mutable
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = P.ModuleName "Data.Array"

  fold
    [ -- fromFoldableImpl
      --   :: forall f a
      --    . (forall b. (a -> b -> b) -> b -> f a -> b)
      --   -> f a
      --   -> Array a
      builtIn @m @(((Value m -> Vector (Value m) -> EvalT m (Vector (Value m))) -> Vector (Value m) -> Value m -> EvalT m (Vector (Value m))) -> Value m -> EvalT m (Vector (Value m)))
        _ModuleName "fromFoldableImpl"
        \_foldr xs ->
          _foldr (\y ys -> pure (Vector.cons y ys)) Vector.empty xs
      -- range :: Int -> Int -> Array Int
    , builtIn @m @(Integer -> Integer -> EvalT m (Vector Integer))
        _ModuleName "range"
        \from to -> 
          pure (Vector.fromList [from..to])
      -- replicate :: forall a. Int -> a -> Array a
    , builtIn @m @(Integer -> Value m -> EvalT m (Vector (Value m)))
        _ModuleName "replicate"
        \n a ->
          pure (Vector.replicate (fromIntegral n) a)
      -- length :: forall a. Array a -> Int
    , builtIn @m @(Vector (Value m) -> EvalT m Integer)
        _ModuleName "length"
        \v ->
          pure (fromIntegral (Vector.length v))
      -- unconsImpl :: forall a b
      --    . (Unit -> b)
      --   -> (a -> Array a -> b)
      --   -> Array a
      --   -> b
    , builtIn @m @((Value m -> EvalT m (Value m)) -> (Value m -> Vector (Value m) -> EvalT m (Value m)) -> Vector (Value m) -> EvalT m (Value m))
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
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Vector (Value m) -> Integer -> EvalT m (Value m))
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
    , builtIn @m @(Value m -> (Value m -> EvalT m Bool) -> (Value m -> EvalT m (Value m)) -> Vector (Value m) -> EvalT m (Value m))
        _ModuleName "findMapImpl"
        \_nothing _isJust _f _xs ->
          throwErrorWithContext (OtherError "findMapImpl is not implemented")
      -- findIndexImpl :: forall a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> (a -> Boolean)
      --   -> Array a
      --   -> Maybe Int
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> (Value m -> EvalT m Bool) -> Vector (Value m) -> EvalT m (Value m))
        _ModuleName "findIndexImpl"
        \_just _nothing _p _xs ->
          throwErrorWithContext (OtherError "findIndexImpl is not implemented")
      -- findLastIndexImpl :: forall a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> (a -> Boolean)
      --   -> Array a
      --   -> Maybe Int
    , builtIn @m @((Integer -> EvalT m (Value m)) -> Value m -> (Value m -> EvalT m Bool) -> Vector (Value m) -> EvalT m (Value m))
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
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Integer -> Value m -> Vector (Value m) -> EvalT m (Value m))
        _ModuleName "_insertAt"
        \_just _nothing _i _x _xs ->
          throwErrorWithContext (OtherError "_insertAt is not implemented")
      -- _deleteAt :: forall a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> Int
      --   -> Array a
      --   -> Maybe (Array a)
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Integer -> Vector (Value m) -> EvalT m (Value m))
        _ModuleName "_deleteAt"
        \_just _nothing _i _xs ->
          throwErrorWithContext (OtherError "_deleteAt is not implemented")
      -- _updateAt :: forall a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> Int
      --   -> a
      --   -> Array a
      --   -> Maybe (Array a)
    , builtIn @m @((Vector (Value m) -> EvalT m (Value m)) -> Value m -> Integer -> Value m -> Vector (Value m) -> EvalT m (Value m))
        _ModuleName "_updateAt"
        \_just _nothing i x xs ->
          if i >= 0 && fromIntegral i < Vector.length xs 
            then _just $ Vector.modify (\mut -> Mutable.write mut (fromIntegral i) x) xs
            else pure _nothing
      -- reverse :: forall a. Array a -> Array a
    , builtIn @m @(Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "reverse"
        \xs ->
          pure (Vector.reverse xs)
      -- concat :: forall a. Array (Array a) -> Array a
    , builtIn @m @(Vector (Vector (Value m)) -> EvalT m (Vector (Value m)))
        _ModuleName "concat"
        \xss ->
          pure (Vector.concat (Vector.toList xss))
      -- filter :: forall a. (a -> Boolean) -> Array a -> Array a
    , builtIn @m @((Value m -> EvalT m Bool) -> Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "filter"
        \p xs ->
          Vector.filterM p xs
      -- partition :: forall a
      --    . (a -> Boolean)
      --   -> Array a
      --   -> { yes :: Array a, no :: Array a }
    , builtIn @m @((Value m -> EvalT m Bool) -> Vector (Value m) -> EvalT m (HashMap Text (Vector (Value m))))
        _ModuleName "partition"
        \p xs ->
          let mkV yes no = HashMap.fromList [("yes", yes), ("no", no)]
           in mkV <$> Vector.filterM p xs <*> Vector.filterM (fmap not . p) xs
      -- scanl :: forall a b. (b -> a -> b) -> b -> Array a -> Array b
    , builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> Value m -> Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "scanl"
        \_f _b _xs ->
          throwErrorWithContext (OtherError "scanl is not implemented")
      -- scanr :: forall a b. (a -> b -> b) -> b -> Array a -> Array b
    , builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> Value m -> Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "scanr"
        \_f _b _xs ->
          throwErrorWithContext (OtherError "scanr is not implemented")
      -- sortByImpl :: forall a. (a -> a -> Ordering) -> (Ordering -> Int) -> Array a -> Array a
    , builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> (Value m -> EvalT m Integer) -> Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "sortByImpl"
        \_f _g _xs ->
          throwErrorWithContext (OtherError "sortByImpl is not implemented")
      -- slice :: forall a. Int -> Int -> Array a -> Array a
    , builtIn @m @(Integer -> Integer -> Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "slice"
        \startAt len xs ->
          pure (Vector.slice (fromIntegral startAt) (fromIntegral len) xs)
      -- zipWith :: forall a b c
      --    . (a -> b -> c)
      --   -> Array a
      --   -> Array b
      --   -> Array c
    , builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> Vector (Value m) -> Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "zipWith"
        \f xs ys ->
          Vector.zipWithM f xs ys
      -- any :: forall a. (a -> Boolean) -> Array a -> Boolean
    , builtIn @m @((Value m -> EvalT m Bool) -> Vector (Value m) -> EvalT m Bool)
        _ModuleName "any"
        \f xs ->
          Vector.or <$> traverse f xs
      -- all :: forall a. (a -> Boolean) -> Array a -> Boolean
      , builtIn @m @((Value m -> EvalT m Bool) -> Vector (Value m) -> EvalT m Bool)
        _ModuleName "all"
        \f xs ->
          Vector.and <$> traverse f xs
      -- unsafeIndexImpl :: forall a. Array a -> Int -> a
    , builtIn @m @(Vector (Value m) -> Integer -> EvalT m (Value m))
        _ModuleName "unsafeIndexImpl"
        \xs i ->
          case xs Vector.!? fromIntegral i of
            Nothing -> throwErrorWithContext (OtherError "unsafeIndexImpl: index out of range")
            Just a -> pure a
    ]
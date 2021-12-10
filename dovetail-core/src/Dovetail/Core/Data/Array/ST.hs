{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Array.ST where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Data.Functor (($>))
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable (IOVector)
import Data.Vector.Mutable qualified as Mutable
import Dovetail
import Dovetail.Core.Control.Monad.ST.Internal (ST)
import Dovetail.Evaluate (ForeignType(..), builtIn)
import Language.PureScript qualified as P

type STArray a = ForeignType (IOVector a)

env :: forall m. (MonadIO m, MonadIO m, Typeable m) => Env m
env = do
  let _ModuleName = P.ModuleName "Data.Array.ST"

  fold
    [ -- unsafeFreeze :: forall h a. STArray h a -> ST h (Array a)
      builtIn @m @(STArray (Value m) -> ST m (Vector (Value m)))
        _ModuleName "unsafeFreeze"
        \(ForeignType v) _ -> 
          liftIO (Vector.freeze v)
      -- unsafeThaw :: forall h a. Array a -> ST h (STArray h a)
    , builtIn @m @(Vector (Value m) -> ST m (STArray (Value m)))
        _ModuleName "unsafeThaw"
        \v _ -> 
          ForeignType <$> liftIO (Vector.thaw v)
      -- new :: forall h a. ST h (STArray h a)
    , builtIn @m @(ST m (STArray (Value m)))
        _ModuleName "new"
        \_ -> 
          ForeignType <$> liftIO (Mutable.new 0)
      -- freeze :: forall h a. STArray h a -> ST h (Array a)
    , builtIn @m @(STArray (Value m) -> ST m (Vector (Value m)))
        _ModuleName "freeze"
        \(ForeignType v) _ -> 
          liftIO (Vector.freeze v)
      -- thaw :: forall h a. Array a -> ST h (STArray h a)
    , builtIn @m @(Vector (Value m) -> ST m (STArray (Value m)))
        _ModuleName "thaw"
        \v _ -> 
          ForeignType <$> liftIO (Vector.thaw v)
      -- shiftImpl :: forall h a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> STArray h a
      --   -> ST h (Maybe a)
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> STArray (Value m) -> ST m (Value m))
        _ModuleName "shiftImpl"
        \_just _nothing _xs _ ->
          throwErrorWithContext (OtherError "shiftImpl is not implemented")
      -- sortByImpl :: forall a h
      --    . (a -> a -> Ordering)
      --   -> (Ordering -> Int)
      --   -> STArray h a
      --   -> ST h (STArray h a)
    , builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> (Value m -> EvalT m Integer) -> STArray (Value m) -> ST m (STArray (Value m)))
        _ModuleName "sortByImpl"
        \_cmp _f _xs _ ->
          throwErrorWithContext (OtherError "sortByImpl is not implemented")
      -- peekImpl :: forall h a r
      --    . (a -> r)
      --   -> r
      --   -> Int
      --   -> STArray h a
      --   -> ST h r
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Integer -> STArray (Value m) -> ST m (Value m))
        _ModuleName "peekImpl"
        \_just _nothing i (ForeignType xs) _ ->
          if i >= 0 && fromIntegral i <= Mutable.length xs 
            then _just =<< liftIO (Mutable.read xs (fromIntegral i))
            else pure _nothing
      -- poke :: forall h a. Int -> a -> STArray h a -> ST h Boolean
    , builtIn @m @(Integer -> Value m -> STArray (Value m) -> ST m Bool)
        _ModuleName "poke"
        \i x (ForeignType xs) _ ->
          if i >= 0 && fromIntegral i <= Mutable.length xs 
            then liftIO (Mutable.write xs (fromIntegral i) x) $> True
            else pure False
      -- popImpl :: forall h a
      --    . (forall b. b -> Maybe b)
      --   -> (forall b. Maybe b)
      --   -> STArray h a
      --   -> ST h (Maybe a)
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> STArray (Value m) -> ST m (Value m))
        _ModuleName "popImpl"
        \_just _nothing _ _ ->
          throwErrorWithContext (OtherError "popImpl is not implemented")
      -- pushAll :: forall h a
      --    . Array a
      --   -> STArray h a
      --   -> ST h Int
    , builtIn @m @(Vector (Value m) -> STArray (Value m) -> ST m Integer)
        _ModuleName "pushAll"
        \_ _ _ ->
          throwErrorWithContext (OtherError "pushAll is not implemented")
      -- unshiftAll :: forall h a
      --    . Array a
      --   -> STArray h a
      --   -> ST h Int
    , builtIn @m @(Vector (Value m) -> STArray (Value m) -> ST m Integer)
        _ModuleName "unshiftAll"
        \_ _ _ ->
          throwErrorWithContext (OtherError "unshiftAll is not implemented")
      -- splice :: forall h a
      --    . Int
      --   -> Int
      --   -> Array a
      --   -> STArray h a
      --   -> ST h (Array a)
    , builtIn @m @(Integer -> Integer -> Vector (Value m) -> STArray (Value m) -> ST m (Vector (Value m)))
        _ModuleName "splice"
        \_ _ _ _ _ ->
          throwErrorWithContext (OtherError "splice is not implemented")
      -- toAssocArray :: forall h a. STArray h a -> ST h (Array (Assoc a))
    , builtIn @m @(STArray (Value m) -> ST m (Vector (Value m)))
        _ModuleName "toAssocArray"
        \_ _ ->
          throwErrorWithContext (OtherError "toAssocArray is not implemented")
    ]
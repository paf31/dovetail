{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold, for_)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

type Effect m a = Value m -> EvalT m a

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Effect"

  fold
    [ -- pureE :: forall a. a -> Effect a
      builtIn @m @(Value m -> Effect m (Value m))
        _ModuleName "pureE" 
        \a _ -> pure a
      -- bindE :: forall a b. Effect a -> (a -> Effect b) -> Effect b
    , builtIn @m @(Effect m (Value m) -> (Value m -> Effect m (Value m)) -> Effect m (Value m))
        _ModuleName "bindE" 
        \e f rw -> do
          a <- e rw
          f a rw
      -- untilE :: Effect Boolean -> Effect Unit
    , builtIn @m @(Effect m Bool -> Effect m (Value m) -> Effect m (Value m))
        _ModuleName "untilE"
        \cond body rw -> do
          let loop = do
                _ <- body rw
                b <- cond rw
                when b loop
          loop
          pure (Object mempty)
      -- whileE :: forall a. Effect Boolean -> Effect a -> Effect Unit
    , builtIn @m @(Effect m Bool -> Effect m (Value m) -> Effect m (Value m))
        _ModuleName "whileE"
        \cond body rw -> do
          let loop = do
                b <- cond rw
                when b (body rw *> loop)
          loop
          pure (Object mempty)
      -- for :: forall r a. Int -> Int -> (Int ->  a) ->  Unit
    , builtIn @m @(Integer -> Integer -> (Integer -> Effect m (Value m)) -> Effect m (Value m))
        _ModuleName "for"
        \from to body rw -> do
          for_ [from .. to] \i -> 
            body i rw
          pure (Object mempty)
      -- foreach :: forall r a. Array a -> (a ->  Unit) ->  Unit
    , builtIn @m @(Vector (Value m) -> (Value m -> Effect m (Value m)) -> Effect m (Value m))
        _ModuleName "foreach"
        \xs f rw -> do
          for_ xs \x -> 
            f x rw
          pure (Object mempty)
    ]

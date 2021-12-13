{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect where

import Control.Monad (unless, when)
import Data.Foldable (fold, for_)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

type Effect ctx a = Value ctx -> Eval ctx a

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Effect"

  fold
    [ -- pureE :: forall a. a -> Effect a
      builtIn @ctx @(Value ctx -> Effect ctx (Value ctx))
        _ModuleName "pureE" 
        \a _ -> pure a
      -- bindE :: forall a b. Effect a -> (a -> Effect b) -> Effect b
    , builtIn @ctx @(Effect ctx (Value ctx) -> (Value ctx -> Effect ctx (Value ctx)) -> Effect ctx (Value ctx))
        _ModuleName "bindE" 
        \e f rw -> do
          a <- e rw
          f a rw
      -- untilE :: Effect Boolean -> Effect Unit
    , builtIn @ctx @(Effect ctx Bool -> Effect ctx (Value ctx))
        _ModuleName "untilE"
        \cond rw -> do
          let loop = do
                b <- cond rw
                unless b loop
          loop
          pure (Object mempty)
      -- whileE :: forall a. Effect Boolean -> Effect a -> Effect Unit
    , builtIn @ctx @(Effect ctx Bool -> Effect ctx (Value ctx) -> Effect ctx (Value ctx))
        _ModuleName "whileE"
        \cond body rw -> do
          let loop = do
                b <- cond rw
                when b (body rw *> loop)
          loop
          pure (Object mempty)
      -- for :: forall r a. Int -> Int -> (Int ->  a) ->  Unit
    , builtIn @ctx @(Integer -> Integer -> (Integer -> Effect ctx (Value ctx)) -> Effect ctx (Value ctx))
        _ModuleName "for"
        \from to body rw -> do
          for_ [from .. to] \i -> 
            body i rw
          pure (Object mempty)
      -- foreach :: forall r a. Array a -> (a ->  Unit) ->  Unit
    , builtIn @ctx @(Vector (Value ctx) -> (Value ctx -> Effect ctx (Value ctx)) -> Effect ctx (Value ctx))
        _ModuleName "foreach"
        \xs f rw -> do
          for_ xs \x -> 
            f x rw
          pure (Object mempty)
    ]

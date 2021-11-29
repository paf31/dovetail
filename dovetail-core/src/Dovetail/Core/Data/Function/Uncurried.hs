{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Function.Uncurried where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.Function.Uncurried"

  fold
    [ -- runFn2 :: forall a b c. Fn2 a b c -> a -> b -> c
      builtIn @m @((Value m -> Value m -> EvalT m (Value m)) -> Value m -> Value m -> EvalT m (Value m))
        _ModuleName "runFn2" 
        id
    ]

-- mkFn0 :: forall a. (Unit -> a) -> Fn0 a
-- 
-- mkFn2 :: forall a b c. (a -> b -> c) -> Fn2 a b c
-- 
-- mkFn3 :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d
-- 
-- mkFn4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e
-- 
-- mkFn5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f
-- 
-- mkFn6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Fn6 a b c d e f g
-- 
-- mkFn7 :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Fn7 a b c d e f g h
-- 
-- mkFn8 :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fn8 a b c d e f g h i
-- 
-- mkFn9 :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Fn9 a b c d e f g h i j
-- 
-- mkFn10 :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Fn10 a b c d e f g h i j k
-- 
-- runFn0 :: forall a. Fn0 a -> a
-- 
-- runFn2 :: forall a b c. Fn2 a b c -> a -> b -> c
-- 
-- runFn3 :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d
-- 
-- runFn4 :: forall a b c d e. Fn4 a b c d e -> a -> b -> c -> d -> e
-- 
-- runFn5 :: forall a b c d e f. Fn5 a b c d e f -> a -> b -> c -> d -> e -> f
-- 
-- runFn6 :: forall a b c d e f g. Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g
-- 
-- runFn7 :: forall a b c d e f g h. Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h
-- 
-- runFn8 :: forall a b c d e f g h i. Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i
-- 
-- runFn9 :: forall a b c d e f g h i j. Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j
-- 
-- runFn10 :: forall a b c d e f g h i j k. Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k
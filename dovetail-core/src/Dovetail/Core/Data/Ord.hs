{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Ord where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Ord"
      
      compareImpl lt eq gt x y = pure
        case compare x y of
          LT -> lt
          EQ -> eq
          GT -> gt

  fold
    [ -- ordBooleanImpl :: Ordering -> Ordering -> Ordering -> Boolean -> Boolean -> Ordering
      builtIn @ctx @(Value ctx -> Value ctx -> Value ctx -> Bool -> Bool -> Eval ctx (Value ctx))
        _ModuleName "ordBooleanImpl" 
        compareImpl
    , -- ordIntImpl :: Ordering -> Ordering -> Ordering -> Int -> Int -> Ordering
      builtIn @ctx @(Value ctx -> Value ctx -> Value ctx -> Integer -> Integer -> Eval ctx (Value ctx))
        _ModuleName "ordIntImpl" 
        compareImpl
    , -- ordNumberImpl :: Ordering -> Ordering -> Ordering -> Double -> Double -> Ordering
      builtIn @ctx @(Value ctx -> Value ctx -> Value ctx -> Double -> Double -> Eval ctx (Value ctx))
        _ModuleName "ordNumberImpl" 
        compareImpl
    , -- ordStringImpl :: Ordering -> Ordering -> Ordering -> Text -> Text -> Ordering
      builtIn @ctx @(Value ctx -> Value ctx -> Value ctx -> Text -> Text -> Eval ctx (Value ctx))
        _ModuleName "ordStringImpl" 
        compareImpl
    , -- ordCharImpl :: Ordering -> Ordering -> Ordering -> Char -> Char -> Ordering
      builtIn @ctx @(Value ctx -> Value ctx -> Value ctx -> Char -> Char -> Eval ctx (Value ctx))
        _ModuleName "ordCharImpl" 
        compareImpl
    , -- ordArrayImpl :: forall a. (a -> a -> Int) -> Array a -> Array a -> Int
      builtIn @ctx @((Value ctx -> Value ctx -> Eval ctx Integer) -> Vector (Value ctx) -> Vector (Value ctx) -> Eval ctx Integer)
        _ModuleName "ordArrayImpl"
        \cmp xs ys ->
          Vector.foldr 
            (\new old -> if new == 0 then old else new) 
            (fromIntegral (Vector.length ys - Vector.length xs)) 
            <$> Vector.zipWithM cmp xs ys
    ]
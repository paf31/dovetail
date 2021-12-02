{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Ord where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.Ord"
      
      compareImpl lt eq gt x y = pure
        case compare x y of
          LT -> lt
          EQ -> eq
          GT -> gt

  fold
    [ -- ordBooleanImpl :: Ordering -> Ordering -> Ordering -> Boolean -> Boolean -> Ordering
      builtIn @m @(Value m -> Value m -> Value m -> Bool -> Bool -> EvalT m (Value m))
        _ModuleName "ordBooleanImpl" 
        compareImpl
    , -- ordIntImpl :: Ordering -> Ordering -> Ordering -> Int -> Int -> Ordering
      builtIn @m @(Value m -> Value m -> Value m -> Integer -> Integer -> EvalT m (Value m))
        _ModuleName "ordIntImpl" 
        compareImpl
    , -- ordNumberImpl :: Ordering -> Ordering -> Ordering -> Double -> Double -> Ordering
      builtIn @m @(Value m -> Value m -> Value m -> Double -> Double -> EvalT m (Value m))
        _ModuleName "ordNumberImpl" 
        compareImpl
    , -- ordStringImpl :: Ordering -> Ordering -> Ordering -> Text -> Text -> Ordering
      builtIn @m @(Value m -> Value m -> Value m -> Text -> Text -> EvalT m (Value m))
        _ModuleName "ordStringImpl" 
        compareImpl
    , -- ordCharImpl :: Ordering -> Ordering -> Ordering -> Char -> Char -> Ordering
      builtIn @m @(Value m -> Value m -> Value m -> Char -> Char -> EvalT m (Value m))
        _ModuleName "ordCharImpl" 
        compareImpl
    , -- ordArrayImpl :: forall a. (a -> a -> Int) -> Array a -> Array a -> Int
      builtIn @m @((Value m -> Value m -> EvalT m Integer) -> Vector (Value m) -> Vector (Value m) -> EvalT m Integer)
        _ModuleName "ordArrayImpl"
        \cmp xs ys ->
          Vector.foldr 
            (\new old -> if new == 0 then old else new) 
            (fromIntegral (Vector.length ys - Vector.length xs)) 
            <$> Vector.zipWithM cmp xs ys
    ]
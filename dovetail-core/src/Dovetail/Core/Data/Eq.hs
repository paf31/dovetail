{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Eq where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Eq"
  
      eqImpl :: Eq a => a -> a -> Eval ctx Bool
      eqImpl x y = pure (x == y)

  fold
    [ -- eqBooleanImpl :: Boolean -> Boolean -> Boolean
      builtIn @ctx @(Bool -> Bool -> Eval ctx Bool)
        _ModuleName "eqBooleanImpl"
        (eqImpl @Bool)
      -- eqIntImpl :: Int -> Int -> Boolean
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Bool)
        _ModuleName "eqIntImpl"
        (eqImpl @Integer)
      -- eqNumberImpl :: Number -> Number -> Boolean
    , builtIn @ctx @(Double -> Double -> Eval ctx Bool)
        _ModuleName "eqNumberImpl"
        (eqImpl @Double)
      -- eqCharImpl :: Char -> Char -> Boolean
    , builtIn @ctx @(Char -> Char -> Eval ctx Bool)
        _ModuleName "eqCharImpl"
        (eqImpl @Char)
      -- eqStringImpl :: String -> String -> Boolean
    , builtIn @ctx @(Text -> Text -> Eval ctx Bool)
        _ModuleName "eqStringImpl"
        (eqImpl @Text)
      -- eqArrayImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean
    , builtIn @ctx @((Value ctx -> Value ctx -> Eval ctx Bool) -> Vector (Value ctx) -> Vector (Value ctx) -> Eval ctx Bool)
        _ModuleName "eqArrayImpl"
        \f xs ys -> 
          if Vector.length xs == Vector.length ys
            then Vector.and <$> Vector.zipWithM f xs ys
            else pure False
    ]

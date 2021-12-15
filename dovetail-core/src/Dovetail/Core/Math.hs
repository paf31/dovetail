{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Math where

import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Math"

  fold
    [ -- abs :: Number -> Number
      builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "abs"
        \a -> 
          pure (abs a)
      -- acos :: Number -> Radians
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "acos"
        \a -> 
          pure (acos a)
      -- asin :: Number -> Radians
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "asin"
        \a -> 
          pure (asin a)
      -- atan :: Number -> Radians
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "atan"
        \a -> 
          pure (atan a)
      -- atan2 :: Number -> Number -> Radians
    , builtIn @ctx @(Double -> Double -> Eval ctx Double)
        _ModuleName "atan2"
        \a b -> 
          pure (atan2 a b)
      -- ceil :: Number -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "ceil"
        \a -> 
          pure (fromIntegral @Integer (ceiling a))
      -- cos :: Radians -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "cos"
        \a -> 
          pure (cos a)
      -- exp :: Number -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "exp"
        \a -> 
          pure (exp a)
      -- floor :: Number -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "floor"
        \a -> 
          pure (fromIntegral @Integer (floor a))
      -- imul :: Int -> Int -> Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "imul"
        \a b ->
          pure (a * b)
      -- log :: Number -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "log"
        \a -> 
          pure (log a)
      -- max :: Number -> Number -> Number
    , builtIn @ctx @(Double -> Double -> Eval ctx Double)
        _ModuleName "max"
        \a b -> 
          pure (max a b)
      -- min :: Number -> Number -> Number
    , builtIn @ctx @(Double -> Double -> Eval ctx Double)
        _ModuleName "min"
        \a b -> 
          pure (min a b)
      -- pow :: Number -> Number -> Number
    , builtIn @ctx @(Double -> Double -> Eval ctx Double)
        _ModuleName "pow"
        \a b -> 
          pure (a ** b)
      -- round :: Number -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "round"
        \a -> 
          pure (fromIntegral @Integer (round a))
      -- sin :: Radians -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "sin"
        \a -> 
          pure (sin a)
      -- sqrt :: Number -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "sqrt"
        \a -> 
          pure (sqrt a)
      -- tan :: Radians -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "tan"
        \a -> 
          pure (tan a)
      -- trunc :: Number -> Number
    , builtIn @ctx @(Double -> Eval ctx Double)
        _ModuleName "trunc"
        \a -> 
          pure (fromIntegral @Integer (truncate a))
      -- remainder :: Number -> Number -> Number
    , builtIn @ctx @(Double -> Double -> Eval ctx Double)
        _ModuleName "remainder"
        \a b -> 
          pure (a - fromIntegral @Integer (truncate (a / b)) * b)
      -- e :: Number
    , builtIn @ctx @Double
        _ModuleName "e"
          (exp 1)
      -- ln2 :: Number
    , builtIn @ctx @Double
        _ModuleName "ln2"
          (log 2)
      -- ln10 :: Number
    , builtIn @ctx @Double
        _ModuleName "ln10"
          (log 10)
      -- log2e :: Number
    , builtIn @ctx @Double
        _ModuleName "log2e"
          (1 / log 2)
      -- log10e :: Number
    , builtIn @ctx @Double
        _ModuleName "log10e"
          (1 /  log 10)
      -- pi :: Number
    , builtIn @ctx @Double
        _ModuleName "pi"
          pi
      -- tau :: Number
    , builtIn @ctx @Double
        _ModuleName "tau"
          (2 * pi)
      -- sqrt1_2 :: Number
    , builtIn @ctx @Double
        _ModuleName "sqrt1_2"
          (sqrt 0.5)
      -- sqrt2 :: Number
    , builtIn @ctx @Double
        _ModuleName "sqrt2"
          (sqrt 2)
    ]
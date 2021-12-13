{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Int.Bits where

import Data.Foldable (fold)
import Data.Bits
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Int.Bits"

  fold
    [ -- and :: Int -> Int -> Int
      builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "and"
        \a b ->
          pure (a .&. b)
      -- or :: Int -> Int -> Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "or"
        \a b ->
          pure (a .|. b)
      -- xor :: Int -> Int -> Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "xor"
        \a b ->
          pure (a `xor` b)
      -- shl :: Int -> Int -> Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "shl"
        \a b ->
          pure (a `shiftL` fromIntegral b)
      -- shr :: Int -> Int -> Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "shr"
        \a b ->
          pure (a `shiftR` fromIntegral b)
      -- zshr :: Int -> Int -> Int
    , builtIn @ctx @(Integer -> Integer -> Eval ctx Integer)
        _ModuleName "zshr"
        \_ _ ->
          throwErrorWithContext (OtherError "zshr is not implemented")
      -- complement :: Int -> Int
    , builtIn @ctx @(Integer -> Eval ctx Integer)
        _ModuleName "complement"
        \a ->
          pure (complement a)
    ]

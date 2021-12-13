{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Enum where

import Data.Char (chr, ord)
import Data.Foldable (fold)
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall ctx. Env ctx
env = do
  let _ModuleName = P.ModuleName "Data.Enum"

  fold
    [ -- toCharCode :: Char -> Int
      builtIn @ctx @(Char -> Eval ctx Integer)
        _ModuleName "toCharCode"
        \c ->
          pure (fromIntegral (ord c))
      -- fromCharCode :: Int -> Char
    , builtIn @ctx @(Integer -> Eval ctx Char)
        _ModuleName "fromCharCode"
        \i ->
          pure (chr (fromIntegral i))
    ]
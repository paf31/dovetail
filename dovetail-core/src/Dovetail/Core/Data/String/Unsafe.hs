{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.Unsafe where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.String.Unsafe"

  fold
    [ -- charAt :: Int -> String -> Char
      builtIn @m @(Integer -> Text -> EvalT m Char)  
        _ModuleName "charAt" 
        \i str -> 
          if i >= 0 && fromIntegral i < Text.length str
            then pure (Text.index str (fromIntegral i))
            else throwErrorWithContext (OtherError "charAt: index out of range")
      -- char :: String -> Char
    , builtIn @m @(Text -> EvalT m Char)
        _ModuleName "char" 
        \str -> 
          if Text.length str == 1
            then pure (Text.head str)
            else throwErrorWithContext (OtherError "char: not a singleton")
    ]
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.Common where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.String.Common"

  fold
    [ builtIn @m @(Value m -> Value m -> Value m -> Text -> Text -> EvalT m (Value m))
        _ModuleName "_localeCompare"
          \lt eq gt t1 t2 -> pure
            case compare t1 t2 of
              LT -> lt
              EQ -> eq
              GT -> gt
      -- replace :: Pattern -> Replacement -> String -> String
    , builtIn @m @(Text -> Text -> Text -> EvalT m Text)
        _ModuleName "replace" 
        \needle replacement haystack ->
          pure case Text.breakOn needle haystack of
                 (_, "") -> haystack
                 (before, after) -> before <> replacement <> after
      -- replaceAll :: Pattern -> Replacement -> String -> String
    , builtIn @m @(Text -> Text -> Text -> EvalT m Text)
        _ModuleName "replaceAll" 
        \needle replacement haystack ->
          pure (Text.replace needle replacement haystack)
      -- split :: Pattern -> String -> Array String
    , builtIn @m @(Text -> Text -> EvalT m (Vector Text))
        _ModuleName "split" 
        \sep s ->
          pure (Vector.fromList (Text.splitOn sep s))
      -- toLower :: String -> String
    , builtIn @m @(Text -> EvalT m Text)
        _ModuleName "toLower" 
        (pure . Text.toLower)
      -- toUpper :: String -> String
    , builtIn @m @(Text -> EvalT m Text)
        _ModuleName "toUpper" 
        (pure . Text.toUpper)
      -- trim :: String -> String
    , builtIn @m @(Text -> EvalT m Text)
        _ModuleName "trim" 
        (pure . Text.strip)
      -- joinWith :: String -> Array String -> String
    , builtIn @m @(Text -> Vector Text -> EvalT m Text)
        _ModuleName "joinWith" 
        \sep ss ->
          pure (Text.intercalate sep (Vector.toList ss))
    ]
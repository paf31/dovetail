{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Record.Builder where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Record.Builder"

  fold
    [ -- copyRecord :: forall r1. Record r1 -> Record r1
      builtIn @ctx @(HashMap Text (Value ctx) -> Eval ctx (HashMap Text (Value ctx)))
        _ModuleName "copyRecord"
        pure
      -- unsafeInsert :: forall a r1 r2. String -> a -> Record r1 -> Record r2
    , builtIn @ctx @(Text -> Value ctx -> HashMap Text (Value ctx) -> Eval ctx (HashMap Text (Value ctx)))
        _ModuleName "unsafeInsert"
        \k v m ->
          pure (HashMap.insert k v m)
      -- unsafeModify :: forall a b r1 r2. String -> (a -> b) -> Record r1 -> Record r2
    , builtIn @ctx @(Text -> (Value ctx -> Eval ctx (Value ctx)) -> HashMap Text (Value ctx) -> Eval ctx (HashMap Text (Value ctx)))
        _ModuleName "unsafeModify"
        \k f m -> do
          case HashMap.lookup k m of
            Just old -> do
              new <- f old
              pure $ HashMap.insert k new m
            Nothing ->
              pure m
      -- unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
    , builtIn @ctx @(Text -> HashMap Text (Value ctx) -> Eval ctx (HashMap Text (Value ctx)))
        _ModuleName "unsafeDelete"
        \k m ->
          pure (HashMap.delete k m)
      -- unsafeRename :: forall r1 r2. String -> String -> Record r1 -> Record r2
    , builtIn @ctx @(Text -> Text -> HashMap Text (Value ctx) -> Eval ctx (HashMap Text (Value ctx)))
        _ModuleName "unsafeRename"
        \k1 k2 m -> do
          case HashMap.lookup k1 m of
            Just v -> do
              pure $ HashMap.insert k2 v (HashMap.delete k1 m)
            Nothing -> 
              pure m
    ]
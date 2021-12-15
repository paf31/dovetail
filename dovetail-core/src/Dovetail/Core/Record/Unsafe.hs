{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Record.Unsafe where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Record.Unsafe"

  fold
    [ -- unsafeHas :: forall r1. String -> Record r1 -> Boolean
      builtIn @ctx @(Text -> HashMap Text (Value ctx) -> Eval ctx Bool)
        _ModuleName "unsafeHas"
        \k r -> 
          pure (HashMap.member k r)
      -- unsafeGet :: forall r a. String -> Record r -> a
    , builtIn @ctx @(Text -> HashMap Text (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "unsafeGet"
        \k r -> 
          case HashMap.lookup k r of
            Nothing -> 
              throwErrorWithContext (OtherError ("unsafeGet: key " <> k <> " did not exist in record"))
            Just a ->
              pure a
      -- unsafeSet :: forall r1 r2 a. String -> a -> Record r1 -> Record r2
    , builtIn @ctx @(Text -> Value ctx -> HashMap Text (Value ctx) -> Eval ctx (HashMap Text (Value ctx)))
        _ModuleName "unsafeSet"
        \k a r ->
          pure (HashMap.insert k a r)
      -- unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
    , builtIn @ctx @(Text -> HashMap Text (Value ctx) -> Eval ctx (HashMap Text (Value ctx)))
        _ModuleName "unsafeDelete"
        \k r -> 
          pure (HashMap.delete k r)
    ]

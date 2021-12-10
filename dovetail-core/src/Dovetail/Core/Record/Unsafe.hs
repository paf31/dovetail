{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Record.Unsafe where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Record.Unsafe"

  fold
    [ -- unsafeHas :: forall r1. String -> Record r1 -> Boolean
      builtIn @m @(Text -> HashMap Text (Value m) -> EvalT m Bool)
        _ModuleName "unsafeHas"
        \k r -> 
          pure (HashMap.member k r)
      -- unsafeGet :: forall r a. String -> Record r -> a
    , builtIn @m @(Text -> HashMap Text (Value m) -> EvalT m (Value m))
        _ModuleName "unsafeGet"
        \k r -> 
          case HashMap.lookup k r of
            Nothing -> 
              throwErrorWithContext (OtherError ("unsafeGet: key " <> k <> " did not exist in record"))
            Just a ->
              pure a
      -- unsafeSet :: forall r1 r2 a. String -> a -> Record r1 -> Record r2
    , builtIn @m @(Text -> Value m -> HashMap Text (Value m) -> EvalT m (HashMap Text (Value m)))
        _ModuleName "unsafeSet"
        \k a r ->
          pure (HashMap.insert k a r)
      -- unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
    , builtIn @m @(Text -> HashMap Text (Value m) -> EvalT m (HashMap Text (Value m)))
        _ModuleName "unsafeDelete"
        \k r -> 
          pure (HashMap.delete k r)
    ]

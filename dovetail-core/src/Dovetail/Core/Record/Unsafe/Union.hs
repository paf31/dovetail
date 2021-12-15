{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Record.Unsafe.Union where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Record.Unsafe.Union"

  -- unsafeUnionFn :: forall r1 r2 r3. Fn2 (Record r1) (Record r2) (Record r3)
  builtIn @ctx @(HashMap Text (Value ctx) -> HashMap Text (Value ctx) -> Eval ctx (HashMap Text (Value ctx)))
    _ModuleName "unsafeUnionFn"
    \r1 r2 ->
      pure (HashMap.union r1 r2)


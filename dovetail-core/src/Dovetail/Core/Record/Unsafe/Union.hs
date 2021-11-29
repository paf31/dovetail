{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Record.Unsafe.Union where

import Control.Monad.Fix (MonadFix)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Record.Unsafe.Union"

  -- unsafeUnionFn :: forall r1 r2 r3. Fn2 (Record r1) (Record r2) (Record r3)
  builtIn @m @(HashMap Text (Value m) -> HashMap Text (Value m) -> EvalT m (HashMap Text (Value m)))
    _ModuleName "unsafeUnionFn"
    \r1 r2 ->
      pure (HashMap.union r1 r2)


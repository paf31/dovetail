{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Record.Builder where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)


env :: forall m. MonadFix m => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

      _ModuleName = ModuleName "Record.Builder"

  fold
    [
    ]

-- copyRecord :: forall r1. Record r1 -> Record r1
-- unsafeInsert :: forall a r1 r2. String -> a -> Record r1 -> Record r2
-- unsafeModify :: forall a b r1 r2. String -> (a -> b) -> Record r1 -> Record r2
-- unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
-- unsafeRename :: forall r1 r2. String -> String -> Record r1 -> Record r2

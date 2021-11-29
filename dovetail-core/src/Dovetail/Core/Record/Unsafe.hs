{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Record.Unsafe where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

      _ModuleName = P.ModuleName "Record.Unsafe"

  fold
    [
    ]

-- unsafeHas :: forall r1. String -> Record r1 -> Boolean
-- 
-- unsafeGet :: forall r a. String -> Record r -> a
-- 
-- unsafeSet :: forall r1 r2 a. String -> a -> Record r1 -> Record r2
-- 
-- unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2

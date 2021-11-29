{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Test.Assert where

import Control.Monad (unless)
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

      _ModuleName = P.ModuleName "Test.Assert"

  fold
    [ -- assertImpl :: String -> Boolean -> Effect Unit
      builtIn @m @(Text -> Bool -> EvalT m (Value m))
        _ModuleName "assertImpl" 
        \message b -> do
          unless b (throwErrorWithContext (OtherError message))
          pure (Object mempty)
    ]

-- checkThrows :: forall a. (Unit -> a) -> Effect Boolean

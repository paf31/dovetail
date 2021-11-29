{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Console where

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

      _ModuleName = P.ModuleName "Effect.Console"

  fold
    [
    ]

-- log :: String -> Effect Unit
-- 
-- warn :: String -> Effect Unit
-- 
-- error :: String -> Effect Unit
-- 
-- info :: String -> Effect Unit
-- 
-- time :: String -> Effect Unit
-- 
-- timeLog :: String -> Effect Unit
-- 
-- timeEnd :: String -> Effect Unit
-- 
-- clear :: Effect Unit

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Show.Generic where

import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Data.Show.Generic"

  -- intercalate :: String -> Array String -> String
  builtIn @m @(Text -> Vector Text -> EvalT m Text)
    _ModuleName "intercalate"
    \sep xs ->
      pure (Text.intercalate sep (Vector.toList xs))
    

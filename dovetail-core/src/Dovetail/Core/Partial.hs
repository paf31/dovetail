{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Partial where

import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Dovetail
import Dovetail.Evaluate (builtIn)


env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = ModuleName "Partial"

  builtIn @m @(Text -> EvalT m (Value m))
    _ModuleName "_crashWith"
    \message -> 
      throwErrorWithContext (OtherError message)
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Test.Assert where

import Control.Monad (unless)
import Control.Monad.Error.Class (catchError)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Data.Text (Text)
import Dovetail
import Dovetail.Core.Effect (Effect)
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Test.Assert"

  fold
    [ -- assertImpl :: String -> Boolean -> Effect Unit
      builtIn @m @(Text -> Bool -> Effect m (Value m))
        _ModuleName "assertImpl" 
        \message b _ -> do
          unless b (throwErrorWithContext (OtherError message))
          pure (Object mempty)
      -- checkThrows :: forall a. (Unit -> a) -> Effect Boolean
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Effect m Bool)
        _ModuleName "checkThrows"
        \f _ ->
          catchError 
            (f (Object mempty) *> pure False)
            \_ -> pure True
    ]

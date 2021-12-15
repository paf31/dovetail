{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Test.Assert where

import Control.Monad (unless)
import Control.Monad.Error.Class (catchError)
import Data.Foldable (fold)
import Data.Text (Text)
import Dovetail
import Dovetail.Core.Effect (Effect)
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Test.Assert"

  fold
    [ -- assertImpl :: String -> Boolean -> Effect Unit
      builtIn @ctx @(Text -> Bool -> Effect ctx (Value ctx))
        _ModuleName "assertImpl" 
        \message b _ -> do
          unless b (throwErrorWithContext (OtherError message))
          pure (Object mempty)
      -- checkThrows :: forall a. (Unit -> a) -> Effect Boolean
    , builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Effect ctx Bool)
        _ModuleName "checkThrows"
        \f _ ->
          catchError 
            (f (Object mempty) *> pure False)
            \_ -> pure True
    ]

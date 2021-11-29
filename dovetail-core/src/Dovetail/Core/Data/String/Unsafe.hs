{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.Unsafe where

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

      _ModuleName = ModuleName "Data.String.Unsafe"

  fold
    [ builtIn @m @(Value m -> EvalT m (Value m))  --  :: Int -> Text -> Char
        _ModuleName "charAt" 
        \_ -> 
          notImplemented "charAt"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Text -> Char
        _ModuleName "char" 
        \_ -> 
          notImplemented "char"
    ]
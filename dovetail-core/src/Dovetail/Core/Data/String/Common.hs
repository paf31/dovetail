{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.Common where

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

      _ModuleName = ModuleName "Data.String.Common"

  fold
    [ builtIn @m @(Value m -> Value m -> Value m -> Text -> Text -> EvalT m (Value m))
        _ModuleName "_localeCompare"
          \lt eq gt t1 t2 -> pure
            case compare t1 t2 of
              LT -> lt
              EQ -> eq
              GT -> gt
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Pattern -> Replacement -> Text -> String
        _ModuleName "replace" 
        \_ -> notImplemented "replace"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Pattern -> Replacement -> Text -> String
        _ModuleName "replaceAll" 
        \_ -> notImplemented "replaceAll"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Pattern -> Text -> Vector String
        _ModuleName "split" 
        \_ -> notImplemented "split"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Text -> String
        _ModuleName "toLower" 
        \_ -> notImplemented "toLower"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Text -> String
        _ModuleName "toUpper" 
        \_ -> notImplemented "toUpper"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Text -> String
        _ModuleName "trim" 
        \_ -> notImplemented "trim"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Text -> Vector Text -> String
        _ModuleName "joinWith" 
        \_ -> notImplemented "joinWith"
    ]
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.CodePoints where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

      _ModuleName = P.ModuleName "Data.String.CodePoints"

  fold
    [ builtIn @m @(Integer -> EvalT m Text)
        _ModuleName "_singleton" 
        \_cp -> 
          notImplemented "_singleton"
    , builtIn @m @(Vector Integer -> EvalT m Text)
        _ModuleName "_fromCodePointArray" 
        \_cps -> 
          notImplemented "_fromCodePointArray"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: 
        _ModuleName "_toCodePointArray" 
        \_ -> notImplemented "_toCodePointArray"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: 
        _ModuleName "_codePointAt" 
        \_ -> notImplemented "_codePointAt"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: 
        _ModuleName "_countPrefix" 
        \_ -> notImplemented "_countPrefix"
    , builtIn @m @((Integer -> Text -> EvalT m Text) -> Integer -> Text -> EvalT m Text)
        _ModuleName "_take" 
        \fallback -> fallback
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: 
        _ModuleName "_unsafeCodePointAt0" 
        \_ -> notImplemented "_unsafeCodePointAt0"
    ]

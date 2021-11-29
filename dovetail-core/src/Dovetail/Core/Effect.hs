{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect where

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

      _ModuleName = ModuleName "Effect"

  fold
    [ -- pureE :: forall a. a -> Effect a
      builtIn @m @(Value m -> Value m -> EvalT m (Value m))
        _ModuleName "pureE" 
        \a _ -> pure a
      -- bindE :: forall a b. Effect a -> (a -> Effect b) -> Effect b
    , builtIn @m @((Value m -> EvalT m (Value m)) -> (Value m -> Value m -> EvalT m (Value m)) -> Value m -> EvalT m (Value m))
        _ModuleName "bindE" 
        \e f rw -> do
          a <- e rw
          f a rw
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Effect Boolean -> Effect Unit
        _ModuleName "untilE" 
        \_ -> notImplemented "untilE"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: forall a. Effect Boolean -> Effect a -> Effect Unit
        _ModuleName "whileE" 
        \_ -> notImplemented "whileE"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: Int -> Int -> (Int -> Effect Unit) -> Effect Unit
        _ModuleName "forE" 
        \_ -> notImplemented "forE"
    , builtIn @m @(Value m -> EvalT m (Value m))  --  :: forall a. Vector a -> (a -> Effect Unit) -> Effect Unit
        _ModuleName "foreachE" 
        \_ -> notImplemented "foreachE"
    ]

-- untilE :: Effect Boolean -> Effect Unit
-- 
-- whileE :: forall a. Effect Boolean -> Effect a -> Effect Unit
-- 
-- forE :: Int -> Int -> (Int -> Effect Unit) -> Effect Unit
-- 
-- foreachE :: forall a. Array a -> (a -> Effect Unit) -> Effect Unit

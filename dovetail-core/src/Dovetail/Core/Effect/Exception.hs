{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Exception where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (ForeignType(..), builtIn)

env :: forall m. (MonadFix m, Typeable m) => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

      _ModuleName = ModuleName "Effect.Exception"

  fold
    [ -- throwException :: forall a. Error -> Effect a
      builtIn @m @(ForeignType (EvaluationError m) -> Value m -> EvalT m (Value m))
        _ModuleName "throwException" 
        \(ForeignType e) _ -> 
          throwErrorWithContext (errorType e)
    , -- catchException :: forall a. (Error -> Effect a) -> Effect a -> Effect a
      builtIn @m @((ForeignType (EvaluationError m) -> Value m -> EvalT m (Value m)) -> (Value m -> EvalT m (Value m)) -> Value m -> EvalT m (Value m))
        _ModuleName "catchException" 
        \c t rw -> 
          catchError (t rw) (\e -> c (ForeignType e) rw)
    ]

-- showErrorImpl :: Error -> String
-- 
-- error :: String -> Error
-- 
-- message :: Error -> String
-- 
-- name :: Error -> String
-- 
-- stackImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Error -> Maybe String


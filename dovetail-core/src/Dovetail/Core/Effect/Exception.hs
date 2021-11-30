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

type Error m = ForeignType (EvaluationError m)

renderValueOptions :: RenderValueOptions
renderValueOptions = RenderValueOptions
  { colorOutput = False
  , maximumDepth = Nothing
  }

env :: forall m. (MonadFix m, Typeable m) => Env m
env = do
  let _ModuleName = ModuleName "Effect.Exception"

  fold
    [ -- throwException :: forall a. Error -> Effect a
      builtIn @m @(Error m -> Value m -> EvalT m (Value m))
        _ModuleName "throwException" 
        \(ForeignType e) _ -> 
          throwErrorWithContext (errorType e)
    , -- catchException :: forall a. (Error -> Effect a) -> Effect a -> Effect a
      builtIn @m @((Error m -> Value m -> EvalT m (Value m)) -> (Value m -> EvalT m (Value m)) -> Value m -> EvalT m (Value m))
        _ModuleName "catchException" 
        \c t rw -> 
          catchError (t rw) (\e -> c (ForeignType e) rw)
      -- showErrorImpl :: Error -> String
    , builtIn @m @(Error m -> EvalT m Text)
        _ModuleName "showErrorImpl"
        \(ForeignType e) ->
          pure (Text.pack (renderEvaluationError renderValueOptions e))
      -- error :: String -> Error
    , builtIn @m @(Text -> EvalT m (Error m))
        _ModuleName "error"
        \msg ->
          pure (ForeignType (EvaluationError (OtherError msg) (EvaluationContext mempty)))
      -- message :: Error -> String
    , builtIn @m @(Error m -> EvalT m Text)
        _ModuleName "message"
        \(ForeignType e) -> 
          pure (Text.pack (renderEvaluationErrorType renderValueOptions (errorType e)))
      -- name :: Error -> String
    , builtIn @m @(Error m -> EvalT m Text)
        _ModuleName "name"
        \_ ->
          pure "Error"
      -- stackImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Error -> Maybe String
    , builtIn @m @((Text -> EvalT m (Value m)) -> Value m -> Error m -> EvalT m (Value m))
        _ModuleName "stackImpl"
        \_just _nothing (ForeignType e) ->
          case getEvaluationContext (errorContext e) of
            [] -> pure _nothing
            stack -> _just (renderEvaluationStack stack)
    ]
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Exception where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader.Class (ask)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Dovetail
import Dovetail.Evaluate (ForeignType(..), builtIn)

type Error ctx = ForeignType (EvaluationError ctx)

renderValueOptions :: RenderValueOptions
renderValueOptions = RenderValueOptions
  { colorOutput = False
  , maximumDepth = Nothing
  }

env :: forall ctx. Typeable ctx => Env ctx
env = do
  let _ModuleName = ModuleName "Effect.Exception"

  fold
    [ -- throwException :: forall a. Error -> Effect a
      builtIn @ctx @(Error ctx -> Value ctx -> Eval ctx (Value ctx))
        _ModuleName "throwException" 
        \(ForeignType e) _ -> 
          throwErrorWithContext (errorType e)
    , -- catchException :: forall a. (Error -> Effect a) -> Effect a -> Effect a
      builtIn @ctx @((Error ctx -> Value ctx -> Eval ctx (Value ctx)) -> (Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> Eval ctx (Value ctx))
        _ModuleName "catchException" 
        \c t rw -> 
          catchError (t rw) (\e -> c (ForeignType e) rw)
      -- showErrorImpl :: Error -> String
    , builtIn @ctx @(Error ctx -> Eval ctx Text)
        _ModuleName "showErrorImpl"
        \(ForeignType e) ->
          pure (Text.pack (renderEvaluationError renderValueOptions e))
      -- error :: String -> Error
    , builtIn @ctx @(Text -> Eval ctx (Error ctx))
        _ModuleName "error"
        \msg -> do
          EvaluationContext _ ctx <- ask
          pure (ForeignType (EvaluationError (OtherError msg) (EvaluationContext mempty ctx)))
      -- message :: Error -> String
    , builtIn @ctx @(Error ctx -> Eval ctx Text)
        _ModuleName "message"
        \(ForeignType e) -> 
          pure (Text.pack (renderEvaluationErrorType renderValueOptions (errorType e)))
      -- name :: Error -> String
    , builtIn @ctx @(Error ctx -> Eval ctx Text)
        _ModuleName "name"
        \_ ->
          pure "Error"
      -- stackImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Error -> Maybe String
    , builtIn @ctx @((Text -> Eval ctx (Value ctx)) -> Value ctx -> Error ctx -> Eval ctx (Value ctx))
        _ModuleName "stackImpl"
        \_just _nothing (ForeignType e) ->
          case callStack (errorContext e) of
            [] -> pure _nothing
            stack -> _just (renderEvaluationStack stack)
    ]
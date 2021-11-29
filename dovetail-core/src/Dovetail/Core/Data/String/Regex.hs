{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.Regex where
  
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
  
      _DataStringRegex = ModuleName "Data.String.Regex"
  
  fold
    [ builtIn @m @(Value m -> EvalT m Text)
        _DataStringRegex "showRegexImpl" 
        \_r -> 
          notImplemented "showRegexImpl"
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Text -> Value m -> Text -> EvalT m (Value m))
        _DataStringRegex "regexImpl" 
        \_just _nothing _str _flags ->
          notImplemented "regexImpl"
    , builtIn @m @(Value m -> EvalT m Text)
        _DataStringRegex "source" 
        \_r -> 
          notImplemented "source"
    , builtIn @m @(Value m -> EvalT m (Value m))
        _DataStringRegex "flagsImpl" 
        \_r -> 
          notImplemented "flagsImpl"
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Value m -> Text -> EvalT m (Value m))
        _DataStringRegex "_match" 
        \_just _nothing _r _str -> 
          notImplemented "_match"
    , builtIn @m @(Value m -> Text -> Text -> EvalT m Text)
        _DataStringRegex "replace" 
        \_r _repl _str ->
          notImplemented "replace"
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Value m -> (Text -> Vector (Value m) -> EvalT m Text) -> Text -> EvalT m Text)
        _DataStringRegex "_replaceBy" 
        \_just _nothing _r _f _str ->
          notImplemented "_replaceBy"
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Value m -> Text -> EvalT m (Value m))
        _DataStringRegex "_search" 
        \_just _nothing _r _str ->
          notImplemented "_search"
    , builtIn @m @(Value m -> Text -> EvalT m (Vector Text))
        _DataStringRegex "split" 
        \_r _str -> 
          notImplemented "split"
    ]
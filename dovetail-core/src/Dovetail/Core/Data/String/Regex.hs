{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.Regex where
  
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall m. MonadIO m => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))
  
      _ModuleName = ModuleName "Data.String.Regex"
  
  fold
    [ -- showRegexImpl :: Regex -> String
      builtIn @m @(Value m -> EvalT m Text)
        _ModuleName "showRegexImpl" 
        \_r -> 
          notImplemented "showRegexImpl"
      -- regexImpl
      --   :: (String -> Either String Regex)
      --   -> (Regex -> Either String Regex)
      --   -> String
      --   -> String
      --   -> Either String Regex
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Text -> Value m -> Text -> EvalT m (Value m))
        _ModuleName "regexImpl" 
        \_just _nothing _str _flags ->
          notImplemented "regexImpl"
      -- source :: Regex -> String
    , builtIn @m @(Value m -> EvalT m Text)
        _ModuleName "source" 
        \_r -> 
          notImplemented "source"
      -- flagsImpl :: Regex -> RegexFlagsRec
    , builtIn @m @(Value m -> EvalT m (Value m))
        _ModuleName "flagsImpl" 
        \_r -> 
          notImplemented "flagsImpl"
      -- test :: Regex -> String -> Boolean
    , builtIn @m @(Value m -> EvalT m (Value m))
        _ModuleName "test"
        \_r -> 
          notImplemented "test"
      -- _match
      --   :: (forall r. r -> Maybe r)
      --   -> (forall r. Maybe r)
      --   -> Regex
      --   -> String
      --   -> Maybe (NonEmptyArray (Maybe String))
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Value m -> Text -> EvalT m (Value m))
        _ModuleName "_match" 
        \_just _nothing _r _str -> 
          notImplemented "_match"
      -- replace :: Regex -> String -> String -> String
    , builtIn @m @(Value m -> Text -> Text -> EvalT m Text)
        _ModuleName "replace" 
        \_r _repl _str ->
          notImplemented "replace"
      -- _replaceBy
      --   :: (forall r. r -> Maybe r)
      --   -> (forall r. Maybe r)
      --   -> Regex
      --   -> (String -> Array (Maybe String) -> String)
      --   -> String
      --   -> String
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Value m -> (Text -> Vector (Value m) -> EvalT m Text) -> Text -> EvalT m Text)
        _ModuleName "_replaceBy" 
        \_just _nothing _r _f _str ->
          notImplemented "_replaceBy"
      -- _search
      --   :: (forall r. r -> Maybe r)
      --   -> (forall r. Maybe r)
      --   -> Regex
      --   -> String
      --   -> Maybe Int
    , builtIn @m @((Value m -> EvalT m (Value m)) -> Value m -> Value m -> Text -> EvalT m (Value m))
        _ModuleName "_search" 
        \_just _nothing _r _str ->
          notImplemented "_search"
      -- split :: Regex -> String -> Array String
    , builtIn @m @(Value m -> Text -> EvalT m (Vector Text))
        _ModuleName "split" 
        \_r _str -> 
          notImplemented "split"
    ]
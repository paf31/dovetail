{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.String.Regex where
  
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let notImplemented :: Text -> Eval ctx a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))
  
      _ModuleName = ModuleName "Data.String.Regex"
  
  fold
    [ -- showRegexImpl :: Regex -> String
      builtIn @ctx @(Value ctx -> Eval ctx Text)
        _ModuleName "showRegexImpl" 
        \_r -> 
          notImplemented "showRegexImpl"
      -- regexImpl
      --   :: (String -> Either String Regex)
      --   -> (Regex -> Either String Regex)
      --   -> String
      --   -> String
      --   -> Either String Regex
    , builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Text -> Value ctx -> Text -> Eval ctx (Value ctx))
        _ModuleName "regexImpl" 
        \_just _nothing _str _flags ->
          notImplemented "regexImpl"
      -- source :: Regex -> String
    , builtIn @ctx @(Value ctx -> Eval ctx Text)
        _ModuleName "source" 
        \_r -> 
          notImplemented "source"
      -- flagsImpl :: Regex -> RegexFlagsRec
    , builtIn @ctx @(Value ctx -> Eval ctx (Value ctx))
        _ModuleName "flagsImpl" 
        \_r -> 
          notImplemented "flagsImpl"
      -- test :: Regex -> String -> Boolean
    , builtIn @ctx @(Value ctx -> Eval ctx (Value ctx))
        _ModuleName "test"
        \_r -> 
          notImplemented "test"
      -- _match
      --   :: (forall r. r -> Maybe r)
      --   -> (forall r. Maybe r)
      --   -> Regex
      --   -> String
      --   -> Maybe (NonEmptyArray (Maybe String))
    , builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> Value ctx -> Text -> Eval ctx (Value ctx))
        _ModuleName "_match" 
        \_just _nothing _r _str -> 
          notImplemented "_match"
      -- replace :: Regex -> String -> String -> String
    , builtIn @ctx @(Value ctx -> Text -> Text -> Eval ctx Text)
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
    , builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> Value ctx -> (Text -> Vector (Value ctx) -> Eval ctx Text) -> Text -> Eval ctx Text)
        _ModuleName "_replaceBy" 
        \_just _nothing _r _f _str ->
          notImplemented "_replaceBy"
      -- _search
      --   :: (forall r. r -> Maybe r)
      --   -> (forall r. Maybe r)
      --   -> Regex
      --   -> String
      --   -> Maybe Int
    , builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Value ctx -> Value ctx -> Text -> Eval ctx (Value ctx))
        _ModuleName "_search" 
        \_just _nothing _r _str ->
          notImplemented "_search"
      -- split :: Regex -> String -> Array String
    , builtIn @ctx @(Value ctx -> Text -> Eval ctx (Vector Text))
        _ModuleName "split" 
        \_r _str -> 
          notImplemented "split"
    ]
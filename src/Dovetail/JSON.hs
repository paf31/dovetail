{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module is temporary and will be replaced or removed in a release shortly.
module Dovetail.JSON 
  ( JSON
  , mkJSON
  
  -- ** Supporting code
  , stdlib
  
  -- ** Serializable types
  , SerializableType(..)
  , tryReifySerializableType
  ) where

import Debug.Trace
import Control.Monad.Fix (MonadFix)  
import Data.Aeson qualified as Aeson
import Data.Align (alignWith)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies, reflect, reify)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.These (These(..))
import Dovetail
import Dovetail.Evaluate qualified as Evaluate
import Language.PureScript qualified as P
import Language.PureScript.Names qualified as Names
import Language.PureScript.Label qualified as Label

newtype JSON t = JSON { getJSON :: Aeson.Value }

mkJSON :: Reifies t SerializableType => Aeson.Value -> EvalT m (JSON t)
mkJSON = error "TODO"

data SerializableType
  = STObject (HashMap Text SerializableType)
  | STArray SerializableType
  | STNullable SerializableType
  | STString
  | STChar
  | STNumber
  | STInt
  | STBool
  deriving (Show, Eq)

stdlib :: MonadFix m => InterpretT m (Module Ann)
stdlib = build
  "module JSON where\n\
  \\n\
  \data Nullable a = Null | NotNull a"

tryReifySerializableType 
  :: forall m r
   . MonadFix m
  => P.SourceType 
  -> (forall t. Reifies t SerializableType => Proxy t -> EvalT m r)
  -> EvalT m r
tryReifySerializableType = \ty f ->
    go ty >>= (`reify` f)
  where
    go :: P.SourceType -> EvalT m SerializableType
    go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Int"))) =
      pure STInt
    go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Number"))) =
      pure STNumber
    go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "String"))) =
      pure STString
    go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Char"))) =
      pure STChar
    go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Boolean"))) =
      pure STBool
    go (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Record"))) ty) = do
      let (knownFields, _unknownFields) = P.rowToSortedList ty
      STObject . HashMap.fromList <$> sequence
        [ (,) <$> Evaluate.evalPSString k <*> go v
        | P.RowListItem _ (Label.Label k) v <- knownFields
        ]
    go (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Array"))) ty) =
      STArray <$> go ty
    go (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "JSON")) (P.ProperName "Nullable"))) ty) =
      STNullable <$> go ty
    go _ =
      Evaluate.throwErrorWithContext (Evaluate.OtherError "type is not serializable")
  
instance forall t m. (MonadFix m, Reifies t SerializableType) => Evaluate.ToValue m (JSON t) where
  toValue = go (reflect (Proxy :: Proxy t)) . getJSON
    where
      go :: SerializableType -> Aeson.Value -> Value m
      go STNullable{} Aeson.Null = 
        Evaluate.Constructor (Names.ProperName "Null") []
      go (STNullable ty) notNull =
        Evaluate.Constructor (Names.ProperName "NotNull") [go ty notNull]
      go STObject{} (Aeson.Object x) =
        Evaluate.Object (fmap (go _) x)
      go (STArray ty) (Aeson.Array x) =
        Evaluate.Array (fmap (go ty) x)
      go STString (Aeson.String x) =
        Evaluate.String x
      go STChar (Aeson.String x) =
        Evaluate.Char (_ x)
      go STNumber (Aeson.Number x) =
        Evaluate.Number (realToFrac x)
      go STInt (Aeson.Number x) =
        Evaluate.Int (_ x)
      go _ (Aeson.Bool x) =
        Evaluate.Bool x
  fromValue = fmap JSON . go (reflect (Proxy :: Proxy t))
    where
      go :: SerializableType -> Value m -> EvalT m Aeson.Value
      go = \case
        STInt -> \case
          Evaluate.Int i -> 
            pure (Aeson.Number (fromIntegral i))
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "integer" other)
        STNumber -> \case
          Evaluate.Number d -> 
            pure (Aeson.Number (realToFrac d))
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "integer" other)
        STString -> \case
          Evaluate.String s -> 
            pure (Aeson.String s)
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "string" other)
        STChar -> \case
          Evaluate.Char c -> 
            pure (Aeson.String (Text.singleton c))
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "char" other)
        STBool -> \case
          Evaluate.Bool b -> 
            pure (Aeson.Bool b)
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "bool" other)
        STObject fieldTypes -> \case
          o@(Evaluate.Object fields) -> do
            let aligner :: These (Text, SerializableType) (Value m) -> EvalT m Aeson.Value
                aligner (This (field, _)) = Evaluate.throwErrorWithContext (Evaluate.FieldNotFound field o)
                aligner (That _) = Evaluate.throwErrorWithContext (Evaluate.OtherError "TODO: unexpected field")
                aligner (These (_, ty') val) = go ty' val
            fmap Aeson.Object . sequence $ alignWith aligner (HashMap.mapWithKey (,) fieldTypes) fields
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "object" other)
        STArray elementType -> \case
          Evaluate.Array xs ->
            Aeson.Array <$> traverse (go elementType) xs
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "array" other)
        STNullable ty -> \case
          Evaluate.Constructor (Names.ProperName "Null") [] ->
            pure Aeson.Null
          Evaluate.Constructor (Names.ProperName "NotNull") [val] ->
            go ty val
          other ->
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "Nullable" other)
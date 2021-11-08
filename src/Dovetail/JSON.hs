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
  ( JSON(..)
  , fromJSON
  , toJSON
  ) where

import Debug.Trace
import Control.Monad.Fix (MonadFix)  
import Data.Aeson qualified as Aeson
import Data.Align (alignWith)
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies, reflect)
import Data.Text (Text)
import Data.These (These(..))
import Dovetail
import Dovetail.Evaluate qualified as Evaluate
import Language.PureScript qualified as P
import Language.PureScript.Names qualified as Names
import Language.PureScript.Label qualified as Label

fromJSON :: Aeson.Value -> Evaluate.Value m
fromJSON (Aeson.Object x) = Evaluate.Object (fmap fromJSON x)
fromJSON (Aeson.Array x) = Evaluate.Array (fmap fromJSON x)
fromJSON (Aeson.String x) = Evaluate.String x
fromJSON (Aeson.Number x) = Evaluate.Number (realToFrac x)
fromJSON (Aeson.Bool x) = Evaluate.Bool x
fromJSON Aeson.Null = Evaluate.String "TODO: null"

toJSON :: Evaluate.Value m -> Maybe Aeson.Value
toJSON (Evaluate.Object x) = Aeson.Object <$> traverse toJSON x
toJSON (Evaluate.Array x) = Aeson.Array <$> traverse toJSON x
toJSON (Evaluate.String x) = pure (Aeson.String x)
toJSON (Evaluate.Number x) = pure (Aeson.Number (realToFrac x))
toJSON (Evaluate.Bool x) = pure (Aeson.Bool x)
toJSON _ = Nothing

newtype JSON t = JSON { getJSON :: Aeson.Value }

-- TypeVar a Text	
-- TypeConstructor a (Qualified (ProperName TypeName))	
-- TypeApp a (Type a) (Type a)	
-- ForAll a Text (Maybe (Kind a)) (Type a) (Maybe SkolemScope)	
-- ConstrainedType a (Constraint a) (Type a)	
-- Skolem a Text Int SkolemScope	
-- rowToSortedList :: Type a -> ([RowListItem a], Type a)
-- rowFromList :: ([RowListItem a], Type a) -> Type a

instance forall t m. (MonadFix m, Reifies t P.SourceType) => Evaluate.ToValue m (JSON t) where
  toValue = go (reflect (Proxy :: Proxy t)) . getJSON
    where
      go :: P.SourceType -> Aeson.Value -> Value m
      go st _ = traceShow st $ String "TODO"
  fromValue = fmap JSON . go (reflect (Proxy :: Proxy t))
    where
      go :: P.SourceType -> Value m -> EvalT m Aeson.Value
      go = \case
        P.TypeConstructor _ (Prim "Int") -> \case
          Evaluate.Int i -> 
            pure (Aeson.Number (fromIntegral i))
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "integer" other)
        P.TypeConstructor _ (Prim "String") -> \case
          Evaluate.String s -> 
            pure (Aeson.String s)
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "string" other)
        P.TypeApp _ (P.TypeConstructor _ (Prim "Record")) ty -> \case
          o@(Evaluate.Object fields) -> do
            let (knownFields, _unknownFields) = P.rowToSortedList ty
            knownFieldsMap <- HashMap.fromList <$> sequence
              [ (\a b -> (a, (a, b))) <$> Evaluate.evalPSString k <*> pure v
              | P.RowListItem _ (Label.Label k) v <- knownFields
              ]
            let aligner :: These (Text, P.SourceType) (Value m) -> EvalT m Aeson.Value
                aligner (This (field, _)) = Evaluate.throwErrorWithContext (Evaluate.FieldNotFound field o)
                aligner (That _) = Evaluate.throwErrorWithContext (Evaluate.OtherError "TODO: unexpected field")
                aligner (These (_, ty') val) = go ty' val
            fmap Aeson.Object . sequence $ alignWith aligner knownFieldsMap fields
          other -> 
            Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "object" other)
          
        other -> \_ -> do
          traceShowM other
          pure $ Aeson.String "TODO"

pattern Prim :: Text -> Qualified (P.ProperName 'Names.TypeName)
pattern Prim name = P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName name)


-- newtype JSON a = JSON { getJSON :: a }
-- 
-- instance (MonadFix m, Aeson.ToJSON a, Aeson.FromJSON a) => Evaluate.ToValue m (JSON a) where
--   toValue = fromJSON . Aeson.toJSON . getJSON
--   fromValue a =
--     case toJSON a of
--       Just value ->
--         case Aeson.fromJSON value of
--           Aeson.Error err -> Evaluate.throwErrorWithContext . Evaluate.OtherError . fromString $ "Invalid JSON: " <> err
--           Aeson.Success json -> pure (JSON json)
--       Nothing -> Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "json" a)

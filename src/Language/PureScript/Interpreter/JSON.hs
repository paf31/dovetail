{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | This module is temporary and will be replaced or removed in a release shortly.
module Language.PureScript.Interpreter.JSON 
  ( JSON(..)
  , fromJSON
  , toJSON
  ) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Fix (MonadFix)  
import Data.Aeson qualified as Aeson
import Data.String (fromString)
import Language.PureScript.Interpreter.Evaluate qualified as Evaluate

fromJSON :: Aeson.Value -> Evaluate.Value m
fromJSON (Aeson.Object x) = Evaluate.Object (fmap fromJSON x)
fromJSON (Aeson.Array x) = Evaluate.Array (fmap fromJSON x)
fromJSON (Aeson.String x) = Evaluate.String x
fromJSON (Aeson.Number x) = Evaluate.Number (realToFrac x)
fromJSON (Aeson.Bool x) = Evaluate.Bool x
fromJSON Aeson.Null = error "TODO: not supported"

toJSON :: Evaluate.Value m -> Maybe Aeson.Value
toJSON (Evaluate.Object x) = Aeson.Object <$> traverse toJSON x
toJSON (Evaluate.Array x) = Aeson.Array <$> traverse toJSON x
toJSON (Evaluate.String x) = pure (Aeson.String x)
toJSON (Evaluate.Number x) = pure (Aeson.Number (realToFrac x))
toJSON (Evaluate.Bool x) = pure (Aeson.Bool x)
toJSON _ = Nothing
 
newtype JSON a = JSON { getJSON :: a }

instance (MonadFix m, Aeson.ToJSON a, Aeson.FromJSON a) => Evaluate.ToValue m (JSON a) where
  toValue = fromJSON . Aeson.toJSON . getJSON
  fromValue a =
    case toJSON a of
      Just value ->
        case Aeson.fromJSON value of
          Aeson.Error err -> throwError . Evaluate.OtherError . fromString $ "Invalid JSON: " <> err
          Aeson.Success json -> pure (JSON json)
      Nothing -> throwError (Evaluate.TypeMismatch "json")

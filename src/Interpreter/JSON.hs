{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Interpreter.JSON 
  ( JSON(..)
  , fromJSON
  , toJSON
  ) where
  
import Control.Exception (throw)
import Data.Aeson qualified as Aeson
import Data.String (fromString)
import Interpreter qualified

fromJSON :: Aeson.Value -> Interpreter.Value m
fromJSON (Aeson.Object x) = Interpreter.Object (fmap fromJSON x)
fromJSON (Aeson.Array x) = Interpreter.Array (fmap fromJSON x)
fromJSON (Aeson.String x) = Interpreter.String x
fromJSON (Aeson.Number x) = Interpreter.Number x
fromJSON (Aeson.Bool x) = Interpreter.Bool x
fromJSON Aeson.Null = Interpreter.Null

toJSON :: Interpreter.Value m -> Maybe Aeson.Value
toJSON (Interpreter.Object x) = Aeson.Object <$> traverse toJSON x
toJSON (Interpreter.Array x) = Aeson.Array <$> traverse toJSON x
toJSON (Interpreter.String x) = pure (Aeson.String x)
toJSON (Interpreter.Number x) = pure (Aeson.Number x)
toJSON (Interpreter.Bool x) = pure (Aeson.Bool x)
toJSON Interpreter.Null = pure Aeson.Null
toJSON _ = Nothing

newtype JSON a = JSON { getJSON :: a }

instance (Monad m, Aeson.FromJSON a) => Interpreter.FromValue m (JSON a) where
  fromValue a =
    case toJSON a of
      Just value ->
        case Aeson.fromJSON value of
          Aeson.Error err -> throw . Interpreter.OtherError . fromString $ "Invalid JSON: " <> err
          Aeson.Success a -> JSON a
      Nothing -> throw (Interpreter.TypeMismatch "json")

instance (Monad m, Aeson.ToJSON a) => Interpreter.ToValue m (JSON a) where
  toValue = fromJSON . Aeson.toJSON . getJSON
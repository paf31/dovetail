{-# LANGUAGE ImportQualifiedPost #-}

module Interpreter.JSON 
  ( JSON(..)
  , fromJSON
  , toJSON
  ) where
  
import Control.Exception (throw)
import Data.Aeson qualified as Aeson
import Data.String (fromString)
import Interpreter qualified

fromJSON :: Aeson.Value -> Interpreter.Value
fromJSON (Aeson.Object x) = Interpreter.Object (fmap fromJSON x)
fromJSON (Aeson.Array x) = Interpreter.Array (fmap fromJSON x)
fromJSON (Aeson.String x) = Interpreter.String x
fromJSON (Aeson.Number x) = Interpreter.Number x
fromJSON (Aeson.Bool x) = Interpreter.Bool x
fromJSON Aeson.Null = Interpreter.Null

toJSON :: Interpreter.Value -> Maybe Aeson.Value
toJSON (Interpreter.Object x) = Aeson.Object <$> traverse toJSON x
toJSON (Interpreter.Array x) = Aeson.Array <$> traverse toJSON x
toJSON (Interpreter.String x) = pure (Aeson.String x)
toJSON (Interpreter.Number x) = pure (Aeson.Number x)
toJSON (Interpreter.Bool x) = pure (Aeson.Bool x)
toJSON Interpreter.Null = pure Aeson.Null
toJSON _ = Nothing

newtype JSON a = JSON { getJSON :: a }

instance Aeson.FromJSON a => Interpreter.FromValue (JSON a) where
  fromValue = pure $ \a -> 
    case toJSON a of
      Just value ->
        case Aeson.fromJSON value of
          Aeson.Error err -> throw . Interpreter.OtherError . fromString $ "Invalid JSON: " <> err
          Aeson.Success a -> JSON a

instance Aeson.ToJSON a => Interpreter.ToValue (JSON a) where
  toValue = pure (fromJSON . Aeson.toJSON . getJSON)
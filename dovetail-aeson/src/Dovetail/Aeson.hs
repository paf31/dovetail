{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Dovetail.Aeson 
  ( evalJSON
  , tryReifySerializableType 
  , stdlib
  , Nullable(..)
  , UnknownJSON(..)
  ) where

import Control.Monad.Fix (MonadFix)  
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Dynamic qualified as Dynamic
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy (Proxy(..))
import Data.Reflection (reifySymbol)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Dovetail
import Dovetail.Evaluate qualified as Evaluate
import Language.PureScript qualified as P
import Language.PureScript.Names qualified as Names
import Language.PureScript.Label qualified as Label

evalJSON 
  :: MonadFix m
  => Maybe ModuleName 
  -> Text
  -> InterpretT m Aeson.Value
evalJSON defaultModuleName expr = do
  (val, ty) <- eval defaultModuleName expr
  liftEvalT $ tryReifySerializableType ty \(_ :: Proxy a) -> 
    Aeson.toJSON <$> (fromValue @_ @a =<< val)

tryReifySerializableType 
  :: forall m r
   . MonadFix m
  => P.SourceType 
  -> ( forall a
     . ( Aeson.FromJSON a
       , Aeson.ToJSON a
       , Evaluate.ToValue m a
       )
     => Proxy a
     -> EvalT m r
     )
  -> EvalT m r 
tryReifySerializableType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Int"))) f =
  f (Proxy :: Proxy Integer)
tryReifySerializableType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Number"))) f =
  f (Proxy :: Proxy Double)
tryReifySerializableType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "String"))) f =
  f (Proxy :: Proxy Text)
tryReifySerializableType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Char"))) f =
  f (Proxy :: Proxy Char)
tryReifySerializableType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Boolean"))) f =
  f (Proxy :: Proxy Bool)
tryReifySerializableType (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Record"))) ty) f = do
  let (knownFields, unknownFields) = P.rowToSortedList ty
  
      go :: P.SourceType -> EvalT m r
      go (P.KindApp _ P.REmpty{} _) =
        tryReifyRecordType knownFields (\(Proxy :: Proxy xs) -> f (Proxy :: Proxy (Record xs)))
      go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "JSON")) (P.ProperName "JSON"))) = 
        tryReifyRecordType knownFields (\(Proxy :: Proxy xs) -> f (Proxy :: Proxy (OpenRecord xs)))
      go _ =
        Evaluate.throwErrorWithContext (Evaluate.OtherError "record type is not serializable")
  go unknownFields
tryReifySerializableType (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Array"))) ty) f =
  tryReifySerializableType ty (\(Proxy :: Proxy a) -> f (Proxy :: Proxy (Vector a)))
tryReifySerializableType (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "JSON")) (P.ProperName "Nullable"))) ty) f =
  tryReifySerializableType ty (\(Proxy :: Proxy a) -> f (Proxy :: Proxy (Nullable a)))
tryReifySerializableType (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "JSON")) (P.ProperName "JSON"))) f =
  f (Proxy :: Proxy UnknownJSON)  
tryReifySerializableType _  _ =
  Evaluate.throwErrorWithContext (Evaluate.OtherError "type is not serializable")

tryReifyRecordType
  :: forall m r
   . MonadFix m
  => [P.RowListItem P.SourceAnn]
  -> ( forall a
     . ( FromJSONObject a
       , ToJSONObject a
       , ToObject m a
       )
     => Proxy a
     -> EvalT m r
     )
  -> EvalT m r 
tryReifyRecordType [] f = 
  f (Proxy :: Proxy Nil)
tryReifyRecordType (P.RowListItem _ (Label.Label k) x : xs) f = do
  t <- Evaluate.evalPSString k
  reifySymbol (Text.unpack t) \(Proxy :: Proxy k) ->
    tryReifySerializableType x \(Proxy :: Proxy x) ->
      tryReifyRecordType xs \(Proxy :: Proxy xs) ->
        f (Proxy :: Proxy (Cons k x xs))

data OpenRecord xs = OpenRecord
  { _knownFields :: xs
  , _allFields :: HashMap Text UnknownJSON
  }

instance FromJSONObject xs => Aeson.FromJSON (OpenRecord xs) where
  parseJSON = Aeson.withObject "object" \o -> 
    OpenRecord <$> parseJSONObject o <*> pure (fmap UnknownJSON o)
  
instance ToJSONObject xs => Aeson.ToJSON (OpenRecord xs) where
  toJSON (OpenRecord xs o) = Aeson.Object (toJSONObject xs <> fmap getUnknownJSON o)

instance (MonadFix m, ToObject m xs) => ToValue m (OpenRecord xs) where
  toValue (OpenRecord xs o) = Evaluate.Object (toObject xs <> fmap toValue o)
  
  fromValue (Evaluate.Object o) = 
    let isUnknownJSON (Evaluate.Foreign dyn)
          | Just{} <- Dynamic.fromDynamic @Aeson.Value dyn = True
        isUnknownJSON _ = False
     in OpenRecord <$> fromObject o <*> traverse fromValue (HashMap.filter isUnknownJSON o)
  fromValue other = 
    Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "object" other)

newtype Record xs = Record { getRecord :: xs }

instance FromJSONObject xs => Aeson.FromJSON (Record xs) where
  parseJSON = fmap Record . Aeson.withObject "object" parseJSONObject
  
instance ToJSONObject xs => Aeson.ToJSON (Record xs) where
  toJSON (Record xs) = Aeson.Object (toJSONObject xs)

instance (MonadFix m, ToObject m xs) => ToValue m (Record xs) where
  toValue = Evaluate.Object . toObject . getRecord
  
  fromValue (Evaluate.Object o) = 
    Record <$> fromObject o 
  fromValue other = 
    Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "object" other)

class FromJSONObject a where
  parseJSONObject :: Aeson.Object -> Aeson.Parser a
  
class ToJSONObject a where
  toJSONObject :: a -> Aeson.Object

class ToObject m a where
  toObject :: a -> HashMap Text (Value m)
  fromObject :: HashMap Text (Value m) -> EvalT m a

data Nil = Nil

instance FromJSONObject Nil where
  parseJSONObject _ = pure Nil
  
instance ToJSONObject Nil where 
  toJSONObject _ = HashMap.empty

instance Monad m => ToObject m Nil where
  toObject _ = HashMap.empty
  fromObject _ = pure Nil

data Cons (k :: Symbol) x xs = Cons x xs
  
instance forall k x xs. (KnownSymbol k, Aeson.FromJSON x, FromJSONObject xs) => FromJSONObject (Cons k x xs) where
  parseJSONObject o =
    let k = symbolVal (Proxy :: Proxy k)
     in Cons <$> o Aeson..: (Text.pack k) <*> parseJSONObject o
  
instance forall k x xs. (KnownSymbol k, Aeson.ToJSON x, ToJSONObject xs) => ToJSONObject (Cons k x xs) where
  toJSONObject (Cons x xs) =
    let k = symbolVal (Proxy :: Proxy k)
     in HashMap.insert (Text.pack k) (Aeson.toJSON x) (toJSONObject xs)

instance forall m k x xs. (KnownSymbol k, ToValue m x, ToObject m xs) => ToObject m (Cons k x xs) where
  toObject (Cons x xs) = do
    let k = symbolVal (Proxy :: Proxy k)
    HashMap.insert (Text.pack k) (toValue x) (toObject xs)
        
  fromObject m = do
    let k = symbolVal (Proxy :: Proxy k)
    case HashMap.lookup (Text.pack k) m of
      Nothing -> 
        Evaluate.throwErrorWithContext (Evaluate.FieldNotFound (Text.pack k) (Evaluate.Object m))
      Just v -> 
        Cons <$> fromValue v <*> fromObject m

newtype Nullable a = Nullable (Maybe a)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via Maybe a

instance ToValue m a => ToValue m (Nullable a) where
  toValue (Nullable Nothing) = 
    Evaluate.Constructor (Names.ProperName "Null") []
  toValue (Nullable (Just a)) = 
    Evaluate.Constructor (Names.ProperName "NotNull") [toValue a]
  
  fromValue (Evaluate.Constructor (Names.ProperName "Null") []) =
    pure (Nullable Nothing)
  fromValue (Evaluate.Constructor (Names.ProperName "NotNull") [val]) =
    Nullable . Just <$> fromValue val
  fromValue other =
    Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "Nullable" other)
    
newtype UnknownJSON = UnknownJSON { getUnknownJSON :: Aeson.Value }
  deriving (Aeson.ToJSON, Aeson.FromJSON) via Aeson.Value
  
instance MonadFix m => ToValue m UnknownJSON where
  toValue = toValue . Evaluate.ForeignType . getUnknownJSON
  fromValue = fmap (UnknownJSON . Evaluate.getForeignType) . fromValue
  
stdlib :: MonadFix m => InterpretT m (Module Ann)
stdlib = build . Text.unlines $
  [ "module JSON where"
  , ""
  , "data Nullable a = Null | NotNull a"
  , ""
  , "foreign import data JSON :: Type"
  ]

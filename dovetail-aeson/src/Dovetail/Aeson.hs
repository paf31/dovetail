{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
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

-- | 
-- This module provides support for using "Dovetail" with @aeson@.
--
-- For certain appliations, it is useful to use JSON as an input or output format,
-- but to let the user decide the exact schema for that JSON data. The simplest way
-- to let the user have control over this from Dovetail is to use PureScript's
-- types to define the serialization functions. That is, just like with generic
-- deriving, our serializers will be inferred from our types. But with Dovetail,
-- we will use the inferred *PureScript* type to synthesize a serializer, not the
-- Haskell types.
--
-- The @query-json@ example in the repository is a good example. The user's program
-- defines a function from JSON inputs to JSON outputs, and the types and format 
-- of the input and output data are determined by the type of the user's program,
-- which is allowed to be a function between any two *serializable* PureScript types.
--
-- Serializable types include primitives (strings, chars, ints, numbers and booleans),
-- records and arrays of other serializable types, and the special 'Nullable' type
-- which is provided for handling serialization of nullable JSON substructures.
--
-- Note: you do not need to use this module if you are working with JSON whose
-- structure is known ahead of time. In that case, you can simply use Dovetail 
-- directly to marshall Haskell data back and forth over the FFI boundary, and
-- Aeson for serialization. This module should be used when the structure is
-- not known ahead of time, because it is controlled by the user.
--
-- The user's program may have a polymorphic type signature. This can happen
-- easily: for example, if the user's program in the @query-json@ example is a
-- record accessor such as @_.foo@, then it will have a polymorphic (indeed, also
-- row-polymorphic) type. We cannot know what JSON data the user will pass as an
-- input to a polymorphic program, and we can't synthesize a specific type for 
-- that input data. So, this module also provides the 'UnknownJSON' type for
-- handling these cases, which is simply a wrapper around Aeson's 'Aeson.Value'
-- type. Since a polymorphic program cannot inspect a value with a polymorphic
-- type (thanks to parametricity), it is safe to make this data accessible to
-- the program in this way. However, this also means that such data will not
-- be visible in the debugger (and instead, will appear as an abstract @<foreign>@ 
-- value).

module Dovetail.Aeson 
  ( 
  -- * Serializable types    
  -- ** Evaluation
    evalJSON
  
  -- ** Type reification
  , Serializable
  , reify 
  
  -- ** Supporting code
  , stdlib
  , Nullable(..)
  , UnknownJSON(..)
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Dynamic qualified as Dynamic
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
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

-- | Evaluate a PureScript expression from source, returning its value as 
-- encoded JSON.
--
-- This function is a convenient counterpart to 'eval' which can be useful 
-- for applications whose output format is JSON.
evalJSON 
  :: Maybe ModuleName 
  -> Text
  -> Interpret ctx Aeson.Value
evalJSON defaultModuleName expr = do
  (val, ty) <- eval defaultModuleName expr
  liftEval $ reify ty \(_ :: Proxy a) -> 
    Aeson.toJSON <$> (fromValue @_ @a =<< val)

-- | A constraint synonym for the constraint our reified types will satisfy:
-- serialization via 'Aeson.ToJSON', deserialization via 'Aeson.FromJSON', and
-- transport via the FFI, via 'Evaluate.ToValue'.
--
-- This synonym is provided just for the convenience of tidying up the type
-- signatures.
type Serializable ctx a =
  ( Aeson.FromJSON a
  , Aeson.ToJSON a
  , Evaluate.ToValue ctx a
  )

-- | Reify a PureScript 'P.SourceType' as a Haskell type which supports
-- transport via the FFI using 'Evaluate.ToValue', and JSON serialization using
-- 'Aeson.ToJSON' and 'Aeson.FromJSON'.
--
-- Just as @DeriveGeneric@ allows us to derive a type-directed serialization
-- method based on the Haskell type of our values, this function allows us to
-- derive a serialization method based on the *PureScript* type of a value.
--
-- This can be useful in more advanced use cases where 'evalJSON' won't suffice.
-- For example, if we want to take data as input from a JSON structure,
-- then we can reify the PureScript type of the domain of a PureScript function.
reify 
  :: forall ctx r
   . P.SourceType 
  -- ^ The PureScript type we wish to reify, for example, from the return value of 'eval'.
  -> (forall a. Serializable ctx a => Proxy a -> Eval ctx r)
  -- ^ The continuation, which will receive a 'Proxy' for the type which has been
  -- reified.
  -> Eval ctx r
reify = go where
  go :: P.SourceType 
     -> (forall a. Serializable ctx a => Proxy a -> Eval ctx r)
     -> Eval ctx r
  go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Int"))) f =
    f (Proxy :: Proxy Integer)
  go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Number"))) f =
    f (Proxy :: Proxy Double)
  go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "String"))) f =
    f (Proxy :: Proxy Text)
  go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Char"))) f =
    f (Proxy :: Proxy Char)
  go (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Boolean"))) f =
    f (Proxy :: Proxy Bool)
  go P.TypeVar{} f =
    f (Proxy :: Proxy UnknownJSON) 
  go (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Record"))) ty) f = do
    let (knownFields, unknownFields) = P.rowToSortedList ty
    case unknownFields of
      P.REmpty{} ->
        goRecord knownFields (\(Proxy :: Proxy xs) -> f (Proxy :: Proxy (Record xs)))
      P.KindApp _ P.REmpty{} _ ->
        goRecord knownFields (\(Proxy :: Proxy xs) -> f (Proxy :: Proxy (Record xs)))
      P.TypeVar{} ->
        goRecord knownFields (\(Proxy :: Proxy xs) -> f (Proxy :: Proxy (OpenRecord xs)))
      _ ->
        Evaluate.throwErrorWithContext (Evaluate.OtherError "record type is not serializable")
  go (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName "Array"))) ty) f =
    go ty (\(Proxy :: Proxy a) -> f (Proxy :: Proxy (Vector a)))
  go (P.TypeApp _ (P.TypeConstructor _ (P.Qualified (Just (P.ModuleName "JSON")) (P.ProperName "Nullable"))) ty) f =
    go ty (\(Proxy :: Proxy a) -> f (Proxy :: Proxy (Nullable a))) 
  go _  _ =
    Evaluate.throwErrorWithContext (Evaluate.OtherError "type is not serializable")

  goRecord
    :: [P.RowListItem P.SourceAnn]
    -> (forall a. (ToJSONObject a, FromJSONObject a, ToObject ctx a) => Proxy a -> Eval ctx r)
    -> Eval ctx r
  goRecord [] f = 
    f (Proxy :: Proxy Nil)
  goRecord (P.RowListItem _ (Label.Label k) x : xs) f = do
    t <- Evaluate.evalPSString k
    reifySymbol (Text.unpack t) \(Proxy :: Proxy k) ->
      go x \(Proxy :: Proxy x) ->
        goRecord xs \(Proxy :: Proxy xs) ->
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

instance ToObject ctx xs => ToValue ctx (OpenRecord xs) where
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

instance ToObject ctx xs => ToValue ctx (Record xs) where
  toValue = Evaluate.Object . toObject . getRecord
  
  fromValue (Evaluate.Object o) = 
    Record <$> fromObject o 
  fromValue other = 
    Evaluate.throwErrorWithContext (Evaluate.TypeMismatch "object" other)

class FromJSONObject a where
  parseJSONObject :: Aeson.Object -> Aeson.Parser a
  
class ToJSONObject a where
  toJSONObject :: a -> Aeson.Object

class ToObject ctx a where
  toObject :: a -> HashMap Text (Value ctx)
  fromObject :: HashMap Text (Value ctx) -> Eval ctx a

data Nil = Nil

instance FromJSONObject Nil where
  parseJSONObject _ = pure Nil
  
instance ToJSONObject Nil where 
  toJSONObject _ = HashMap.empty

instance ToObject ctx Nil where
  toObject _ = HashMap.empty
  fromObject _ = pure Nil

data Cons (k :: Symbol) x xs = Cons x xs
  
instance forall k x xs. (KnownSymbol k, Aeson.FromJSON x, FromJSONObject xs) => FromJSONObject (Cons k x xs) where
  parseJSONObject o =
    let k = symbolVal (Proxy :: Proxy k)
     in Cons <$> parseMissingAsNull o (Text.pack k) <*> parseJSONObject o
  
parseMissingAsNull :: Aeson.FromJSON x => Aeson.Object -> Text -> Aeson.Parser x
parseMissingAsNull o k =
  Aeson.parseJSON $ fromMaybe Aeson.Null (HashMap.lookup k o)
    
instance forall k x xs. (KnownSymbol k, Aeson.ToJSON x, ToJSONObject xs) => ToJSONObject (Cons k x xs) where
  toJSONObject (Cons x xs) =
    let k = symbolVal (Proxy :: Proxy k)
     in HashMap.insert (Text.pack k) (Aeson.toJSON x) (toJSONObject xs)

instance forall ctx k x xs. (KnownSymbol k, ToValue ctx x, ToObject ctx xs) => ToObject ctx (Cons k x xs) where
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

-- | A representation of nullable values for use in derived serializers.
--
-- See 'reify' and 'stdlib'.
newtype Nullable a = Nullable (Maybe a)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via Maybe a

instance ToValue ctx a => ToValue ctx (Nullable a) where
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
    
-- | A representation of arbitrary JSON values for use in derived serializers.
--
-- This type is reified to stand in for any polymorphic type variables in a 
-- PureScript type, since we cannot know the structure of values of those types
-- ahead of time.
--
-- See 'reify' and 'stdlib'.
newtype UnknownJSON = UnknownJSON { getUnknownJSON :: Aeson.Value }
  deriving (Aeson.ToJSON, Aeson.FromJSON) via Aeson.Value
  
instance ToValue ctx UnknownJSON where
  toValue = toValue . Evaluate.ForeignType . getUnknownJSON
  fromValue = fmap (UnknownJSON . Evaluate.getForeignType) . fromValue
  
-- | This action makes a module named @JSON@ available to your PureScript code.
--
-- It defines the PureScript counterpart of the 'Nullable' type, which is used
-- to serialize nullable types when deriving serializers using 'reify'.
--
-- Any PureScript code which needs to support type-directed serialization for
-- values which may involve @null@ should import this module.
stdlib :: Interpret ctx (Module Ann)
stdlib = build . Text.unlines $
  [ "module JSON where"
  , ""
  , "data Nullable a = Null | NotNull a"
  ]

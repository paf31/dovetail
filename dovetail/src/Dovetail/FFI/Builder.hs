{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | This module provides a higher-level API on top of the 
-- "Dovetail.FFI" module. It is not as expressive as the
-- functions in that module, but has the benefit that it is much harder to use
-- this module to construct an FFI which will result in runtime errors, since
-- it attempts to synthesize the types of the Haskell implementations from the
-- types of the declared PureScript foreign imports.
module Dovetail.FFI.Builder
  ( 
  -- * FFI Builder API
    FFIBuilder
  , runFFIBuilder
  , evalFFIBuilder
  , foreignImport
  
  -- * Supported FFI types
  , FunctionType
  , string
  , char
  , boolean
  , number
  , int
  , array
  , (~>)
  , ForAll
  ) where
  
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Writer.Strict (Writer, runWriter)
import Data.Text (Text)
import Data.Vector (Vector)
import Dovetail.Evaluate (EvalT, Value)
import Dovetail.Evaluate qualified as Evaluate
import Dovetail.FFI (FFI(..), ForeignImport(..))
import Dovetail.FFI.Internal qualified as Internal
import Language.PureScript qualified as P

data TypeScheme m a where
  Cons :: (FunctionType m (Value m) (EvalT m (Value m)) -> TypeScheme m a)
       -> TypeScheme m a
  Nil :: FunctionType m a r -> TypeScheme m a
            
data FunctionType m l r where
  Function  :: FunctionType m al ar
            -> FunctionType m bl br 
            -> FunctionType m (al -> br) (al -> br)
  Array    :: FunctionType m l r 
           -> FunctionType m (Vector l) (EvalT m (Vector l))
  MonoType :: MonoType m l -> FunctionType m l (EvalT m l)
  
data MonoType m a where
  String   :: MonoType m Text
  Char     :: MonoType m Char
  Boolean  :: MonoType m Bool
  Number   :: MonoType m Double
  Int      :: MonoType m Integer
  Var      :: P.SourceType -> MonoType m (Value m)
  
-- | This type class exists to facilitate the concise description of
-- PureScript type schemes using the 'foreignImport' function.
-- It is best understood via its examples:
--
-- @
-- foreignImport (Ident "identity") \a -> a ~> a
--   :: MonadIO m 
--   => (Value m -> EvalT m (Value m)) 
--   -> FFIBuilder m ()
--
-- foreignImport (Ident "flip") \a b c -> (a ~> b ~> c) ~> b ~> a ~> c
--   :: MonadIO m 
--   => ((Value m -> Value m -> EvalT m (Value m))
--   ->   Value m -> Value m -> EvalT m (Value m))
--   -> FFIBuilder m ()
-- @
--
-- These Haskell functions applications describe the PureScript type schemes for the 
-- @identity@ and @flip@ functions respectively.
--
-- Notice that the result type of these applications indicates the corresponding
-- Haskell type which must be implemented in order to satisfy the contract of the
-- FFI. Note, these types have been are inferred, which highlights why this 
-- type class is worth its seeming complexity: the goal is to allow the user to
-- express the PureScript type, and have the compiler compute the Haskell type for
-- us. This is about as simple as things can get - we cannot simply specify the
-- Haskell implementation and infer the PureScript type, because there is not a
-- single best PureScript type for every given Haskell type.
class ForAll m r a | a -> m r where
  
  -- | Create a 'TypeScheme' which describes a PureScript type from a Haskell 
  -- function, where type bindings in PureScript types are represented by
  -- function arguments in the Haskell code.
  forAll :: a -> TypeScheme m r
  
instance ForAll m a (FunctionType m a r_) where
  forAll = Nil
  
instance (ForAll m r o, a ~ FunctionType m (Value m) (EvalT m (Value m))) => ForAll m r (a -> o) where
  forAll f = Cons (forAll . f)
  
infixr 0 ~>

-- | Construct a PureScript function type
(~>) :: FunctionType m al ar
      -> FunctionType m bl br 
      -> FunctionType m (al -> br) (al -> br)
(~>) = Function
  
-- | The PureScript string type
string  :: FunctionType m Text (EvalT m Text)
string = MonoType String
  
-- | The PureScript char type
char  :: FunctionType m Char (EvalT m Char)
char = MonoType Char

-- | The PureScript boolean type
boolean :: FunctionType m Bool (EvalT m Bool)
boolean = MonoType Boolean

-- | The PureScript number type
number :: FunctionType m Double (EvalT m Double)
number = MonoType Number

-- | The PureScript integer type
int :: FunctionType m Integer (EvalT m Integer)
int = MonoType Int
  
-- | Construct a PureScript array type
array :: FunctionType m l r
      -> FunctionType m (Vector l) (EvalT m (Vector l))
array = Array
  
data ForeignImports m = ForeignImports
  { foreignImports_values :: [ForeignImport m]
  }
  
instance Semigroup (ForeignImports m) where
  x <> y = ForeignImports
    { foreignImports_values = foreignImports_values x <> foreignImports_values y
    }
  
instance Monoid (ForeignImports m) where
  mempty = ForeignImports 
    { foreignImports_values = mempty 
    }
  
-- | A monad for constructing 'FFI' data structures.
--
-- For example:
--
-- @
-- FFI.'evalFFIBuilder' ('P.ModuleName' \"Example\") do
--   FFI.'foreignImport' (P.Ident \"example\")
--     (\a -> a ~> a)
--     pure
-- @
newtype FFIBuilder m a = FFIBuilder { unFFIBuilder :: Writer (ForeignImports m) a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter (ForeignImports m)) 
  
-- | Run a computation in the 'FFIBuilder' monad, returning only the constructed
-- 'FFI'.
evalFFIBuilder :: P.ModuleName -> FFIBuilder m a -> FFI m
evalFFIBuilder mn = snd . runFFIBuilder mn
  
-- | Run a computation in the 'FFIBuilder' monad, returning the result of the
-- computation alongside the constructed 'FFI'.
runFFIBuilder :: P.ModuleName -> FFIBuilder m a -> (a, FFI m)
runFFIBuilder mn = fmap convert . runWriter . unFFIBuilder where
  convert (ForeignImports values) = FFI
    { ffi_moduleName = mn
    , ffi_values = values 
    }
  
-- | Define a value which will be implemented in Haskell.
--
-- The first argument gives a name to the value on the PureScript side.
-- 
-- The second argument is a function which describes its PureScript type.
-- See 'ForAll' for an explanation of its purpose.
--
-- The final argument is the Haskell implementation of the value.
--
-- The type checker will ensure that the PureScript and Haskell types are
-- compatible.
foreignImport 
  :: (MonadIO m, Evaluate.ToValue m a, ForAll m a ty)
  => P.Ident
  -> ty
  -> a
  -> FFIBuilder m ()
foreignImport = 
  \nm ty impl -> tell $ ForeignImports
    { foreignImports_values = 
        [ ForeignImport
            { fv_name = nm
            , fv_type = typeSchemeToSourceType (forAll ty)
            , fv_value = Evaluate.toValue impl
            }
        ]
    }
    
typeSchemeToSourceType :: MonadIO m => TypeScheme m a -> P.SourceType
typeSchemeToSourceType (Cons f) = Internal.forAll \a -> typeSchemeToSourceType (f (MonoType (Var a)))
typeSchemeToSourceType (Nil t) = functionTypeToSourceType t

functionTypeToSourceType :: MonadIO m => FunctionType m l r -> P.SourceType
functionTypeToSourceType (Function ty1 ty2) = 
  Internal.function 
    (functionTypeToSourceType ty1)
    (functionTypeToSourceType ty2)
functionTypeToSourceType (Array ty) =
  Internal.array
    (functionTypeToSourceType ty)
functionTypeToSourceType (MonoType t) = monoTypeToSourceType t

monoTypeToSourceType :: MonadIO m => MonoType m a -> P.SourceType
monoTypeToSourceType String = P.tyString
monoTypeToSourceType Char = P.tyChar
monoTypeToSourceType Boolean = P.tyBoolean
monoTypeToSourceType Number = P.tyNumber
monoTypeToSourceType Int = P.tyInt
monoTypeToSourceType (Var a) = a

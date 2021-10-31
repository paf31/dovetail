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

module Language.PureScript.Interpreter.FFI.Builder
  ( 
  -- * FFI Builder API
    FFIBuilder
  , runFFIBuilder
  , evalFFIBuilder
  , foreignImport
  
  -- * Supported FFI types
  , FunctionType
  , string
  , boolean
  , number
  , array
  , (~>)
  , ForAll
  ) where
  
import Control.Monad.Fix (MonadFix)
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Writer.Strict (Writer, runWriter)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import Language.PureScript qualified as P
import Language.PureScript.Interpreter.Evaluate (EvalT, Value)
import Language.PureScript.Interpreter.Evaluate qualified as Evaluate
import Language.PureScript.Interpreter.FFI (FFI(..), ForeignImport(..))
import Language.PureScript.Interpreter.FFI.Internal qualified as Internal

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
  Boolean  :: MonoType m Bool
  Number   :: MonoType m Scientific
  Var      :: P.SourceType -> MonoType m (Value m)
  
class ForAll m r a | a -> m r where
  forAll :: a -> TypeScheme m r
  
instance ForAll m a (FunctionType m a r_) where
  forAll = Nil
  
instance (ForAll m r o, a ~ FunctionType m (Value m) (EvalT m (Value m))) => ForAll m r (a -> o) where
  forAll f = Cons (forAll . f)
  
infixr 0 ~>
(~>) :: FunctionType m al ar
      -> FunctionType m bl br 
      -> FunctionType m (al -> br) (al -> br)
(~>) = Function
  
string  :: FunctionType m Text (EvalT m Text)
string = MonoType String

boolean :: FunctionType m Bool (EvalT m Bool)
boolean = MonoType Boolean

number :: FunctionType m Scientific (EvalT m Scientific)
number = MonoType Number
  
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
  
newtype FFIBuilder m a = FFIBuilder { unFFIBuilder :: Writer (ForeignImports m) a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter (ForeignImports m)) 
  
runFFIBuilder :: P.ModuleName -> FFIBuilder m a -> (a, FFI m)
runFFIBuilder mn = fmap convert . runWriter . unFFIBuilder where
  convert (ForeignImports values) = FFI
    { ffi_moduleName = mn
    , ffi_values = values 
    }
  
evalFFIBuilder :: P.ModuleName -> FFIBuilder m a -> FFI m
evalFFIBuilder mn = snd . runFFIBuilder mn
  
foreignImport 
  :: (MonadFix m, Evaluate.ToValue m a, ForAll m a ty)
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
    
typeSchemeToSourceType :: MonadFix m => TypeScheme m a -> P.SourceType
typeSchemeToSourceType (Cons f) = Internal.forAll \a -> typeSchemeToSourceType (f (MonoType (Var a)))
typeSchemeToSourceType (Nil t) = functionTypeToSourceType t

functionTypeToSourceType :: MonadFix m => FunctionType m l r -> P.SourceType
functionTypeToSourceType (Function ty1 ty2) = 
  Internal.function 
    (functionTypeToSourceType ty1)
    (functionTypeToSourceType ty2)
functionTypeToSourceType (Array ty) =
  Internal.array
    (functionTypeToSourceType ty)
functionTypeToSourceType (MonoType t) = monoTypeToSourceType t

monoTypeToSourceType :: MonadFix m => MonoType m a -> P.SourceType
monoTypeToSourceType String = P.tyString
monoTypeToSourceType Boolean = P.tyBoolean
monoTypeToSourceType Number = P.tyNumber
monoTypeToSourceType (Var a) = a

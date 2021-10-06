{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Interpreter.Prelude where
  
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.PureScript qualified as P
import Language.PureScript.Interpreter (EvalT, FFI(..), Value)
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Types.Extra ((-->), array, forAll)

data FFIType m l r where
  ForAll   :: (FFIType m (Value m) (EvalT m (Value m)) -> FFIType m l r)
           -> FFIType m l r
  Function :: FFIType m al ar
           -> FFIType m bl br 
           -> FFIType m (al -> br) (al -> br)
           
  String   :: FFIType m Text (EvalT m Text)
  Boolean  :: FFIType m Bool (EvalT m Bool)
  Number   :: FFIType m Scientific (EvalT m Scientific)
  Array    :: FFIType m l r
           -> FFIType m (Vector l) (EvalT m (Vector l))
  Var      :: P.SourceType
           -> FFIType m (Value m) (EvalT m (Value m))

infixr 0 ~~>
(~~>) :: FFIType m al ar
      -> FFIType m bl br 
      -> FFIType m (al -> br) (al -> br)
(~~>) = Function
  
mkFFI :: (MonadFix m, Interpreter.ToValue m l) => P.Ident -> FFIType m l r -> l -> (P.Ident, P.SourceType, Value m)
mkFFI = 
    \nm ty impl -> (nm, go ty, Interpreter.toValue impl)
  where
    go :: MonadFix m => FFIType m l r -> P.SourceType
    go String = P.tyString
    go Boolean = P.tyBoolean
    go Number = P.tyNumber
    go (Array ty) = array (go ty)
    go (Function ty1 ty2) = go ty1 --> go ty2
    go (ForAll f) = forAll \a -> go (f (Var a))
    go (Var st) = st

prelude :: forall m. MonadFix m => FFI m
prelude = 
  FFI (P.ModuleName "Prelude")
    [ mkFFI (P.Ident "map") 
        (ForAll \a -> ForAll \b -> (a ~~> b) ~~> Array a ~~> Array b)
        traverse
    , mkFFI (P.Ident "filter") 
        (ForAll \a -> (a ~~> Boolean) ~~> Array a ~~> Array a)
        Vector.filterM
    , mkFFI (P.Ident "foldl") 
        (ForAll \a -> ForAll \b -> (b ~~> a ~~> b) ~~> b ~~> Array a ~~> b)
        Vector.foldM
    , mkFFI (P.Ident "zipWithM") 
        (ForAll \a -> ForAll \b -> ForAll \c -> (a ~~> b ~~> c) ~~> Array a ~~> Array b ~~> Array c)
        Vector.zipWithM
    , mkFFI (P.Ident "append")
        (ForAll \a -> Array a ~~> Array a ~~> Array a)
        (\xs ys -> pure (xs <> ys))
        
    , mkFFI (P.Ident "appendString")
        (String ~~> String ~~> String)
        (\xs ys -> pure (xs <> ys))
        
    , mkFFI (P.Ident "not")
        (Boolean ~~> Boolean)
        (pure . not)
    ]
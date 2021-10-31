{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A tiny standard library.
module Dovetail.Prelude where
  
import Control.Monad.Fix (MonadFix)
import Data.Char (chr, ord)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Dovetail.Evaluate (EvalT, ToValue, ToValueRHS)
import Dovetail.FFI (FFI(..))
import Dovetail.FFI.Builder (array, boolean, char, int, string, number, (~>))
import Dovetail.FFI.Builder qualified as FFI
import Language.PureScript qualified as P

stdlib :: MonadFix m => [FFI m]
stdlib = 
  [ prelude
  , preludeArray
  , preludeString
  , preludeChar
  , preludeNumber
  , preludeInt
  , preludeBoolean
  ]

prelude :: MonadFix m => FFI m
prelude = FFI.evalFFIBuilder (P.ModuleName "Prelude") do
  FFI.foreignImport (P.Ident "identity") 
    (\a -> a ~> a)
    pure
  FFI.foreignImport (P.Ident "flip") 
    (\a b c -> (a ~> b ~> c) ~> b ~> a ~> c)
    flip
    
preludeArray :: MonadFix m => FFI m
preludeArray = FFI.evalFFIBuilder (P.ModuleName "Prelude.Array") do
  FFI.foreignImport (P.Ident "map") 
    (\a b -> (a ~> b) ~> array a ~> array b)
    traverse
  FFI.foreignImport (P.Ident "filter") 
    (\a -> (a ~> boolean) ~> array a ~> array a)
    Vector.filterM
  FFI.foreignImport (P.Ident "foldl") 
    (\a b -> (b ~> a ~> b) ~> b ~> array a ~> b)
    Vector.foldM
  FFI.foreignImport (P.Ident "zipWith") 
    (\a b c -> (a ~> b ~> c) ~> array a ~> array b ~> array c)
    Vector.zipWithM
  FFI.foreignImport (P.Ident "append")
    (\a -> array a ~> array a ~> array a)
    (\xs ys -> pure (xs <> ys))
  
preludeString :: MonadFix m => FFI m
preludeString = FFI.evalFFIBuilder (P.ModuleName "Prelude.String") do
  FFI.foreignImport (P.Ident "append")
    (string ~> string ~> string)
    (\xs ys -> pure (xs <> ys))
  FFI.foreignImport (P.Ident "singleton")
    (char ~> string)
    (pure . Text.singleton)
    
preludeChar :: MonadFix m => FFI m
preludeChar = FFI.evalFFIBuilder (P.ModuleName "Prelude.Char") do
  FFI.foreignImport (P.Ident "chr")
    (int ~> char)
    (pure . chr . fromIntegral)
  FFI.foreignImport (P.Ident "ord")
    (char ~> int)
    (pure . fromIntegral . ord)
    
preludeNumber :: MonadFix m => FFI m
preludeNumber = FFI.evalFFIBuilder (P.ModuleName "Prelude.Number") do
  numOps number
  ordOps number

  FFI.foreignImport (P.Ident "div")
    (number ~> number ~> number)
    (\x y -> pure (x / y))

  FFI.foreignImport (P.Ident "floor")
    (number ~> int)
    (pure . floor)
  FFI.foreignImport (P.Ident "ceiling")
    (number ~> int)
    (pure . ceiling)
  FFI.foreignImport (P.Ident "round")
    (number ~> int)
    (pure . round)
  FFI.foreignImport (P.Ident "truncate")
    (number ~> int)
    (pure . truncate)
  
preludeInt :: MonadFix m => FFI m
preludeInt = FFI.evalFFIBuilder (P.ModuleName "Prelude.Int") do
  numOps int
  ordOps int

  FFI.foreignImport (P.Ident "div")
    (int ~> int ~> int)
    (\x y -> pure (x `div` y))

  FFI.foreignImport (P.Ident "toNumber")
    (int ~> number)
    (pure . fromIntegral)
  
preludeBoolean :: MonadFix m => FFI m
preludeBoolean = FFI.evalFFIBuilder (P.ModuleName "Prelude.Boolean") do
  FFI.foreignImport (P.Ident "and")
    (boolean ~> boolean ~> boolean)
    (\x y -> pure (x && y))
  FFI.foreignImport (P.Ident "or")
    (boolean ~> boolean ~> boolean)
    (\x y -> pure (x || y))
  FFI.foreignImport (P.Ident "not")
    (boolean ~> boolean)
    (pure . not)

numOps 
  :: (ToValue m a, ToValueRHS m (EvalT m a), Num a)
  => FFI.FunctionType m a (EvalT m a)
  -> FFI.FFIBuilder m ()
numOps ty = do
  FFI.foreignImport (P.Ident "add")
    (ty ~> ty ~> ty)
    (\x y -> pure (x + y))
  FFI.foreignImport (P.Ident "sub")
    (ty ~> ty ~> ty)
    (\x y -> pure (x - y))
  FFI.foreignImport (P.Ident "mul")
    (ty ~> ty ~> ty)
    (\x y -> pure (x * y))
    
ordOps 
  :: (ToValue m a, ToValueRHS m (EvalT m a), Ord a)
  => FFI.FunctionType m a (EvalT m a)
  -> FFI.FFIBuilder m ()
ordOps ty = do
  FFI.foreignImport (P.Ident "min")
    (ty ~> ty ~> ty)
    (\x y -> pure (x `min` y))
  FFI.foreignImport (P.Ident "max")
    (ty ~> ty ~> ty)
    (\x y -> pure (x `max` y))

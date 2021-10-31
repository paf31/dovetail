{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A tiny standard library.
module Language.PureScript.Interpreter.Prelude where
  
import Control.Monad.Fix (MonadFix)
import Data.Vector qualified as Vector
import Language.PureScript qualified as P
import Language.PureScript.Interpreter.FFI (FFI(..))
import Language.PureScript.Interpreter.FFI.Builder (array, boolean, string, number, (~>))
import Language.PureScript.Interpreter.FFI.Builder qualified as FFI

stdlib :: MonadFix m => [FFI m]
stdlib = 
  [ prelude
  , preludeArray
  , preludeString
  , preludeNumber
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
    
preludeNumber :: MonadFix m => FFI m
preludeNumber = FFI.evalFFIBuilder (P.ModuleName "Prelude.Number") do
  FFI.foreignImport (P.Ident "add")
    (number ~> number ~> number)
    (\x y -> pure (x + y))
  FFI.foreignImport (P.Ident "subtract")
    (number ~> number ~> number)
    (\x y -> pure (x - y))
  FFI.foreignImport (P.Ident "multiply")
    (number ~> number ~> number)
    (\x y -> pure (x * y))
  FFI.foreignImport (P.Ident "divide")
    (number ~> number ~> number)
    (\x y -> pure (x / y))
  FFI.foreignImport (P.Ident "min")
    (number ~> number ~> number)
    (\x y -> pure (x `min` y))
  FFI.foreignImport (P.Ident "max")
    (number ~> number ~> number)
    (\x y -> pure (x `max` y))
  
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

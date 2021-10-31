{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Interpreter.Prelude where
  
import Control.Monad.Fix (MonadFix)
import Data.Vector qualified as Vector
import Language.PureScript qualified as P
import Language.PureScript.Interpreter.FFI (FFI(..))
import Language.PureScript.Interpreter.FFI.Builder (array, boolean, string, number, (~>))
import Language.PureScript.Interpreter.FFI.Builder qualified as FFI

-- | A tiny standard library.
prelude :: MonadFix m => FFI m
prelude = FFI.evalFFIBuilder (P.ModuleName "Prelude") do
  FFI.foreignImport (P.Ident "identity") 
    (\a -> a ~> a)
    pure
  FFI.foreignImport (P.Ident "flip") 
    (\a b c -> (a ~> b ~> c) ~> b ~> a ~> c)
    flip
    
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
  
  FFI.foreignImport (P.Ident "appendString")
    (string ~> string ~> string)
    (\xs ys -> pure (xs <> ys))
    
  FFI.foreignImport (P.Ident "add")
    (number ~> number ~> number)
    (\x y -> pure (x + y))
  FFI.foreignImport (P.Ident "sub")
    (number ~> number ~> number)
    (\x y -> pure (x - y))
  FFI.foreignImport (P.Ident "mul")
    (number ~> number ~> number)
    (\x y -> pure (x * y))
  FFI.foreignImport (P.Ident "div")
    (number ~> number ~> number)
    (\x y -> pure (x / y))
  FFI.foreignImport (P.Ident "min")
    (number ~> number ~> number)
    (\x y -> pure (x `min` y))
  FFI.foreignImport (P.Ident "max")
    (number ~> number ~> number)
    (\x y -> pure (x `max` y))
  
  FFI.foreignImport (P.Ident "not")
    (boolean ~> boolean)
    (pure . not)

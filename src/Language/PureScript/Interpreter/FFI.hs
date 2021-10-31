{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}

module Language.PureScript.Interpreter.FFI 
  ( 
  -- * Foreign function interface
    FFI(..)
  , ForeignImport(..)
  , toEnv
  , toExterns
  ) where

import Data.Map qualified as Map  
import Language.PureScript qualified as P
import Language.PureScript.Externs qualified as Externs
import Language.PureScript.Interpreter.Types 
  
data FFI m = FFI
  { ffi_moduleName :: P.ModuleName
  , ffi_values :: [ForeignImport m]
  }
  
data ForeignImport m = ForeignImport
  { fv_name :: P.Ident
  , fv_type :: P.SourceType
  , fv_value :: Value m
  }

toExterns :: FFI m -> P.ExternsFile
toExterns (FFI mn vals) =
  Externs.ExternsFile   
    { Externs.efVersion      = "0.14.2"
    , Externs.efModuleName   = mn
    , Externs.efExports      = [P.ValueRef P.nullSourceSpan name | ForeignImport name _ _ <- vals]
    , Externs.efImports      = [ P.ExternsImport (P.ModuleName "Prim") P.Implicit (Just (P.ModuleName "Prim"))
                               , P.ExternsImport (P.ModuleName "Prim") P.Implicit Nothing
                               ]
    , Externs.efFixities     = []
    , Externs.efTypeFixities = []
    , Externs.efDeclarations = [Externs.EDValue name ty | ForeignImport name ty _ <- vals]
    , Externs.efSourceSpan   = P.nullSourceSpan
    } 

toEnv :: FFI m -> Env m
toEnv (FFI mn vals) = 
  Map.fromList [ (P.mkQualified name mn, val) | ForeignImport name _ val <- vals ]
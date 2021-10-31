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
  
-- | Describes a module which is implemented in Haskell, and made available
-- to PureScript code using its foreign function interface. 
--
-- Right now, this consists only of foreign value declarations, even though
-- the FFI supports other forms of interop.
--
-- Values of this type can be constructed directly, but in many cases it is
-- simpler to use the "Language.PureScript.Interpreter.FFI.Builder" module
-- instead.
--
-- Values of this type can be consumed by the 'toExterns' and 'toEnv' functions,
-- and their results passed to the PureScript APIs or the low-level functions in
-- "Language.PureScript.Interpreter.Evaluate" and "Language.PureScript.Make.Simplified", 
-- directly, but it is more likely that you will use values of this type with the 
-- higher-level 'Language.PureScript.Interpreter.ffi' function.
data FFI m = FFI
  { ffi_moduleName :: P.ModuleName
  -- ^ The module name for the module being implemented in Haskell.
  , ffi_values :: [ForeignImport m]
  -- ^ A list of values implemented in Haskell in this module.
  }
  
-- | A single value implemented in a foreign Haskell module.
data ForeignImport m = ForeignImport
  { fv_name :: P.Ident
  -- ^ The name of this value in PureScript code
  , fv_type :: P.SourceType
  -- ^ The PureScript type of this value
  , fv_value :: Value m
  -- ^ The value itself
  }

-- | Convert a foreign module into a PureScript externs file, for use during
-- separate compilation.
--
-- For advanced use cases, the result may be used with the functions in the 
-- "Language.PureScript.Make.Simplified" module.
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

-- | Convert a foreign module into an evaluation environment.
--
-- For advanced use cases, the result may be used with the functions in the 
-- "Language.PureScript.Interpreter.Evaluate" module.
toEnv :: FFI m -> Env m
toEnv (FFI mn vals) = 
  Map.fromList [ (P.mkQualified name mn, val) | ForeignImport name _ val <- vals ]
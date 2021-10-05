{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Language.PureScript.Interpreter.FFI where

import Data.List ((\\), nub)
import Data.Text (Text)
import Data.Text qualified as T
import Language.PureScript qualified as P

typeVars :: [Text]
typeVars = map T.singleton ['a'..'z'] <> map (<> "'") typeVars

boundTypeVars :: P.Type ann -> [Text]
boundTypeVars = nub . P.everythingOnTypes (++) go where
  go (P.ForAll _ name _ _ _) = [name]
  go _ = []

forAll :: (P.SourceType -> P.SourceType) -> P.SourceType
forAll f = 
    P.ForAll P.nullSourceAnn name
      (Just (P.TypeConstructor P.nullSourceAnn (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName {P.runProperName = "Type"})))) 
      (f (P.TypeVar P.nullSourceAnn name))
      Nothing
  where
    name = head (typeVars \\ boundTypeVars (f (P.TypeVar P.nullSourceAnn undefined)))

infixr 0 -->
(-->) :: P.SourceType -> P.SourceType -> P.SourceType
a --> b = 
  P.TypeApp P.nullSourceAnn 
    (P.TypeApp P.nullSourceAnn 
      (P.TypeConstructor P.nullSourceAnn (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName {P.runProperName = "Function"}))) 
      a) 
    b

boolean :: P.SourceType
boolean = primCon "Boolean"

string :: P.SourceType
string = primCon "String"

array :: P.SourceType -> P.SourceType
array a = P.TypeApp P.nullSourceAnn (primCon "Array") a

primCon :: Text -> P.SourceType
primCon nm = P.TypeConstructor P.nullSourceAnn (P.Qualified (Just (P.ModuleName "Prim")) (P.ProperName nm))
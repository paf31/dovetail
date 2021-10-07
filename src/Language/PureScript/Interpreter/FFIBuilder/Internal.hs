{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Language.PureScript.Interpreter.FFIBuilder.Internal
  ( forAll
  , array
  , function
  ) where

import Data.List ((\\), nub)
import Data.Text (Text)
import Data.Text qualified as T
import Language.PureScript qualified as P

forAll :: (P.SourceType -> P.SourceType) -> P.SourceType
forAll f = 
    P.mkForAll 
      [(P.nullSourceAnn, (name, (Just P.kindType)))]
      (f (P.TypeVar P.nullSourceAnn name))
  where
    name = head (typeVars \\ boundTypeVars (f (P.TypeVar P.nullSourceAnn undefined)))

typeVars :: [Text]
typeVars = map T.singleton ['a'..'z'] <> map (<> "'") typeVars

boundTypeVars :: P.Type ann -> [Text]
boundTypeVars = nub . P.everythingOnTypes (++) go where
  go (P.ForAll _ name _ _ _) = [name]
  go _ = []

function :: P.SourceType -> P.SourceType -> P.SourceType
function a b = P.TypeApp P.nullSourceAnn (P.TypeApp P.nullSourceAnn P.tyFunction a) b

array :: P.SourceType -> P.SourceType
array = P.TypeApp P.nullSourceAnn P.tyArray
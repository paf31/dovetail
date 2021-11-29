{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Show where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = P.ModuleName "Data.Show"
  
      showImpl :: forall a. Show a => a -> EvalT m Text
      showImpl = pure . Text.pack . show @a

  fold
    [ -- showIntImpl :: Int -> String
      builtIn @m @(Integer -> EvalT m Text)
        _ModuleName "showIntImpl"
        (showImpl @Integer)
    , -- showNumberImpl :: Number -> String
      builtIn @m @(Double -> EvalT m Text)
        _ModuleName "showNumberImpl"
        (showImpl @Double)
    , -- showCharImpl :: Char -> String
      builtIn @m @(Char -> EvalT m Text)
        _ModuleName "showCharImpl"
        (showImpl @Char)
    , -- showStringImpl :: String -> String
      builtIn @m @(Text -> EvalT m Text)
        _ModuleName "showStringImpl"
        (showImpl @Text)
    , -- showArrayImpl :: forall a. (a -> String) -> Array a -> String
      builtIn @m @((Value m -> EvalT m Text) -> Vector (Value m) -> EvalT m Text)
        _ModuleName "showArrayImpl"
        \_show xs -> 
          fmap 
            (\ys -> "[" <> Text.intercalate "," (Vector.toList ys) <> "]")
            (traverse _show xs)
    , -- cons :: forall a. a -> Array a -> Array a
      builtIn @m @(Value m -> Vector (Value m) -> EvalT m (Vector (Value m)))
        _ModuleName "cons"
        \hd tl ->
          pure (Vector.cons hd tl)
    , -- join :: String -> Array String -> String
      builtIn @m @(Text -> Vector Text -> EvalT m Text)
        _ModuleName "join"
        \sep xs ->
          pure (Text.intercalate sep (Vector.toList xs))
    ]

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Show where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Show"
  
      showImpl :: forall a. Show a => a -> Eval ctx Text
      showImpl = pure . Text.pack . show @a

  fold
    [ -- showIntImpl :: Int -> String
      builtIn @ctx @(Integer -> Eval ctx Text)
        _ModuleName "showIntImpl"
        (showImpl @Integer)
    , -- showNumberImpl :: Number -> String
      builtIn @ctx @(Double -> Eval ctx Text)
        _ModuleName "showNumberImpl"
        (showImpl @Double)
    , -- showCharImpl :: Char -> String
      builtIn @ctx @(Char -> Eval ctx Text)
        _ModuleName "showCharImpl"
        (showImpl @Char)
    , -- showStringImpl :: String -> String
      builtIn @ctx @(Text -> Eval ctx Text)
        _ModuleName "showStringImpl"
        (showImpl @Text)
    , -- showArrayImpl :: forall a. (a -> String) -> Array a -> String
      builtIn @ctx @((Value ctx -> Eval ctx Text) -> Vector (Value ctx) -> Eval ctx Text)
        _ModuleName "showArrayImpl"
        \_show xs -> 
          fmap 
            (\ys -> "[" <> Text.intercalate "," (Vector.toList ys) <> "]")
            (traverse _show xs)
    , -- cons :: forall a. a -> Array a -> Array a
      builtIn @ctx @(Value ctx -> Vector (Value ctx) -> Eval ctx (Vector (Value ctx)))
        _ModuleName "cons"
        \hd tl ->
          pure (Vector.cons hd tl)
    , -- join :: String -> Array String -> String
      builtIn @ctx @(Text -> Vector Text -> Eval ctx Text)
        _ModuleName "join"
        \sep xs ->
          pure (Text.intercalate sep (Vector.toList xs))
    ]

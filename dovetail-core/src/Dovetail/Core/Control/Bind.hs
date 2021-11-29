{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Control.Bind where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let _ModuleName = P.ModuleName "Control.Bind"

  fold
    [ -- arrayBind :: forall a b. Array a -> (a -> Array b) -> Array b
      builtIn @m @(Vector (Value m) -> (Value m -> EvalT m (Vector (Value m))) -> EvalT m (Vector (Value m)))
        _ModuleName "arrayBind"
        \xs f -> Vector.concat <$> traverse f (Vector.toList xs)
    ]

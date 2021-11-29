{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Uncurried where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

      _ModuleName = P.ModuleName "Effect.Uncurried"

  fold
    [
    ]

-- mkEffectFn1 :: forall a r. (a -> Effect r) -> EffectFn1 a r
-- 
-- mkEffectFn2 :: forall a b r. (a -> b -> Effect r) -> EffectFn2 a b r
-- 
-- mkEffectFn3 :: forall a b c r. (a -> b -> c -> Effect r) -> EffectFn3 a b c r
-- 
-- mkEffectFn4 :: forall a b c d r. (a -> b -> c -> d -> Effect r) -> EffectFn4 a b c d r
-- 
-- mkEffectFn5 :: forall a b c d e r. (a -> b -> c -> d -> e -> Effect r) -> EffectFn5 a b c d e r
-- 
-- mkEffectFn6 :: forall a b c d e f r. (a -> b -> c -> d -> e -> f -> Effect r) -> EffectFn6 a b c d e f r
-- 
-- mkEffectFn7 :: forall a b c d e f g r. (a -> b -> c -> d -> e -> f -> g -> Effect r) -> EffectFn7 a b c d e f g r
-- 
-- mkEffectFn8 :: forall a b c d e f g h r. (a -> b -> c -> d -> e -> f -> g -> h -> Effect r) -> EffectFn8 a b c d e f g h r
-- 
-- mkEffectFn9 :: forall a b c d e f g h i r. (a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect r) -> EffectFn9 a b c d e f g h i r
-- 
-- mkEffectFn10 :: forall a b c d e f g h i j r. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Effect r) -> EffectFn10 a b c d e f g h i j r
-- 
-- runEffectFn1 :: forall a r. EffectFn1 a r -> a -> Effect r
-- 
-- runEffectFn2 :: forall a b r. EffectFn2 a b r -> a -> b -> Effect r
-- 
-- runEffectFn3 :: forall a b c r. EffectFn3 a b c r -> a -> b -> c -> Effect r
-- 
-- runEffectFn4 :: forall a b c d r. EffectFn4 a b c d r -> a -> b -> c -> d -> Effect r
-- 
-- runEffectFn5 :: forall a b c d e r. EffectFn5 a b c d e r -> a -> b -> c -> d -> e -> Effect r
-- 
-- runEffectFn6 :: forall a b c d e f r. EffectFn6 a b c d e f r -> a -> b -> c -> d -> e -> f -> Effect r
-- 
-- runEffectFn7 :: forall a b c d e f g r. EffectFn7 a b c d e f g r -> a -> b -> c -> d -> e -> f -> g -> Effect r
-- 
-- runEffectFn8 :: forall a b c d e f g h r. EffectFn8 a b c d e f g h r -> a -> b -> c -> d -> e -> f -> g -> h -> Effect r
-- 
-- runEffectFn9 :: forall a b c d e f g h i r. EffectFn9 a b c d e f g h i r -> a -> b -> c -> d -> e -> f -> g -> h -> i -> Effect r
-- 
-- runEffectFn10 :: forall a b c d e f g h i j r. EffectFn10 a b c d e f g h i j r -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Effect r
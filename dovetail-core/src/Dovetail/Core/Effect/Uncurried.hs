{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}

module Dovetail.Core.Effect.Uncurried where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Data.Text (Text)
import Dovetail
import Dovetail.Core.Effect (Effect)
import Dovetail.Evaluate (builtIn)
import GHC.TypeLits (Nat, type (-))

type family Fn (n :: Nat) (m :: * -> *) where
  Fn 1 m = Value m -> Effect m (Value m)
  Fn n m = Value m -> Fn (n - 1) m

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Effect.Uncurried"

      effectFn :: forall (n :: Nat)
                . (ToValue m (Fn n m), ToValueRHS m (Fn n m))
               => Text
               -> Env m
      effectFn name = builtIn @m @(Fn n m -> Fn n m) _ModuleName name id

  fold
    [ effectFn @1 "runEffectFn1"
    , effectFn @2 "runEffectFn2"
    , effectFn @3 "runEffectFn3"
    , effectFn @4 "runEffectFn4"
    , effectFn @5 "runEffectFn5"
    , effectFn @6 "runEffectFn6"
    , effectFn @7 "runEffectFn7"
    , effectFn @8 "runEffectFn8"
    , effectFn @9 "runEffectFn9"
    , effectFn @10 "runEffectFn10"
    
    , effectFn @1 "mkEffectFn1"
    , effectFn @2 "mkEffectFn2"
    , effectFn @3 "mkEffectFn3"
    , effectFn @4 "mkEffectFn4"
    , effectFn @5 "mkEffectFn5"
    , effectFn @6 "mkEffectFn6"
    , effectFn @7 "mkEffectFn7"
    , effectFn @8 "mkEffectFn8"
    , effectFn @9 "mkEffectFn9"
    , effectFn @10 "mkEffectFn10"
    ]
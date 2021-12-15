{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Dovetail.Core.Effect.Uncurried where

import Data.Foldable (fold)
import Data.Text (Text)
import Dovetail
import Dovetail.Core.Effect (Effect)
import Dovetail.Evaluate (builtIn)
import GHC.TypeLits (Nat, type (-))

type family Fn (n :: Nat) (ctx :: *) where
  Fn 1 ctx = Value ctx -> Effect ctx (Value ctx)
  Fn n ctx = Value ctx -> Fn (n - 1) ctx

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Effect.Uncurried"

      effectFn :: forall (n :: Nat)
                . (ToValue ctx (Fn n ctx), ToValueRHS ctx (Fn n ctx))
               => Text
               -> Env ctx
      effectFn name = builtIn @ctx @(Fn n ctx -> Fn n ctx) _ModuleName name id

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
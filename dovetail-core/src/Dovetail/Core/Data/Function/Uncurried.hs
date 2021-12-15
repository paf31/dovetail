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

module Dovetail.Core.Data.Function.Uncurried where

import Data.Foldable (fold)
import Data.Text (Text)
import Dovetail
import Dovetail.Evaluate (builtIn)
import GHC.TypeLits (Nat, type (-))

type family Fn (n :: Nat) (ctx :: *) where
  Fn 1 ctx = Value ctx -> Eval ctx (Value ctx)
  Fn n ctx = Value ctx -> Fn (n - 1) ctx

env :: forall ctx. Env ctx
env = do
  let _ModuleName = ModuleName "Data.Function.Uncurried"

      fn :: forall (n :: Nat)
          . (ToValue ctx (Fn n ctx), ToValueRHS ctx (Fn n ctx))
         => Text
         -> Env ctx
      fn name = builtIn @ctx @(Fn n ctx -> Fn n ctx) _ModuleName name id

  fold
    [ fn @1 "runFn0"
    , fn @2 "runFn2"
    , fn @3 "runFn3"
    , fn @4 "runFn4"
    , fn @5 "runFn5"
    , fn @6 "runFn6"
    , fn @7 "runFn7"
    , fn @8 "runFn8"
    , fn @9 "runFn9"
    , fn @10 "runFn10"
    
    , fn @1 "mkFn0"
    , fn @2 "mkFn2"
    , fn @3 "mkFn3"
    , fn @4 "mkFn4"
    , fn @5 "mkFn5"
    , fn @6 "mkFn6"
    , fn @7 "mkFn7"
    , fn @8 "mkFn8"
    , fn @9 "mkFn9"
    , fn @10 "mkFn10"
    ]
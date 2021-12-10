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

module Dovetail.Core.Data.Function.Uncurried where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Data.Text (Text)
import Dovetail
import Dovetail.Evaluate (builtIn)
import GHC.TypeLits (Nat, type (-))

type family Fn (n :: Nat) (m :: * -> *) where
  Fn 1 m = Value m -> EvalT m (Value m)
  Fn n m = Value m -> Fn (n - 1) m

env :: forall m. MonadIO m => Env m
env = do
  let _ModuleName = ModuleName "Data.Function.Uncurried"

      fn :: forall (n :: Nat)
          . (ToValue m (Fn n m), ToValueRHS m (Fn n m))
         => Text
         -> Env m
      fn name = builtIn @m @(Fn n m -> Fn n m) _ModuleName name id

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
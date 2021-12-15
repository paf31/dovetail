{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Dovetail.Core.Effect.Ref where

import Control.Monad.Fix (mfix)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (fold)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Typeable (Typeable)
import Dovetail
import Dovetail.Evaluate (ForeignType(..), builtIn)
import GHC.Generics (Generic)

type Ref ctx = ForeignType (IORef (Value ctx))

data ModifyResult ctx = ModifyResult
  { state :: Value ctx
  , value :: Value ctx 
  } deriving Generic
  
instance ToValue ctx (ModifyResult ctx) 

env :: forall ctx. Typeable ctx => Env ctx
env = do
  let _ModuleName = ModuleName "Effect.Ref"

  fold
    [ -- new :: forall s. s -> Effect (Ref s)
      builtIn @ctx @(Value ctx -> Value ctx -> Eval ctx (Ref ctx))
        _ModuleName "new"
        \s _ -> 
          ForeignType <$> liftIO (IORef.newIORef s)
      -- newWithSelf :: forall s. (Ref s -> s) -> Effect (Ref s)
    , builtIn @ctx @((Ref ctx -> Eval ctx (Value ctx)) -> Value ctx -> Eval ctx (Ref ctx))
        _ModuleName "newWithSelf"
        \f _ -> 
          mfix \r -> do
            s <- f r
            ForeignType <$> liftIO (IORef.newIORef s)
      -- read :: forall s. Ref s -> Effect s
    , builtIn @ctx @(Ref ctx -> Value ctx -> Eval ctx (Value ctx))
        _ModuleName "read"
        \(ForeignType ref) _ ->
          liftIO (IORef.readIORef ref)
      -- write :: forall s. s -> Ref s -> Effect Unit
    , builtIn @ctx @(Value ctx -> Ref ctx -> Value ctx -> Eval ctx (Value ctx))
        _ModuleName "write"
        \s (ForeignType ref) _ -> do
          liftIO (IORef.writeIORef ref s)
          pure (Object mempty)
      -- modifyImpl :: forall s b.           
      --   (s                  
      --    -> { state :: s    
      --       , value :: b    
      --       }               
      --   )                   
      --   -> Ref s -> Effect b
    , builtIn @ctx @((Value ctx -> Eval ctx (ModifyResult ctx)) -> Ref ctx -> Value ctx -> Eval ctx (Value ctx))
        _ModuleName "modifyImpl"
        \f (ForeignType ref) _ -> do
          s <- liftIO (IORef.readIORef ref)
          ModifyResult newState result <- f s
          liftIO (IORef.writeIORef ref newState)
          pure result
    ]


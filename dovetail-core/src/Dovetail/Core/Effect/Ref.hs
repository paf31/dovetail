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

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (fold)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Typeable (Typeable)
import Dovetail
import Dovetail.Evaluate (ForeignType(..), builtIn)
import GHC.Generics (Generic)

type Ref m = ForeignType (IORef (Value m))

data ModifyResult m = ModifyResult
  { state :: Value m
  , value :: Value m 
  } deriving Generic
  
instance MonadIO m => ToValue m (ModifyResult m) 

env :: forall m. (MonadIO m, MonadIO m, Typeable m) => Env m
env = do
  let _ModuleName = ModuleName "Effect.Ref"

  fold
    [ -- new :: forall s. s -> Effect (Ref s)
      builtIn @m @(Value m -> Value m -> EvalT m (Ref m))
        _ModuleName "new"
        \s _ -> 
          ForeignType <$> liftIO (IORef.newIORef s)
      -- newWithSelf :: forall s. (Ref s -> s) -> Effect (Ref s)
    , builtIn @m @((Ref m -> EvalT m (Value m)) -> Value m -> EvalT m (Ref m))
        _ModuleName "newWithSelf"
        \f _ -> 
          error "bad"
      -- read :: forall s. Ref s -> Effect s
    , builtIn @m @(Ref m -> Value m -> EvalT m (Value m))
        _ModuleName "read"
        \(ForeignType ref) _ ->
          liftIO (IORef.readIORef ref)
      -- write :: forall s. s -> Ref s -> Effect Unit
    , builtIn @m @(Value m -> Ref m -> Value m -> EvalT m (Value m))
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
    , builtIn @m @((Value m -> EvalT m (ModifyResult m)) -> Ref m -> Value m -> EvalT m (Value m))
        _ModuleName "modifyImpl"
        \f (ForeignType ref) _ -> do
          s <- liftIO (IORef.readIORef ref)
          ModifyResult newState result <- f s
          liftIO (IORef.writeIORef ref newState)
          pure result
    ]


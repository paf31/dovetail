{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Control.Monad.ST.Internal where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (fold, for_)
import Data.IORef qualified as IORef
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Core.Effect.Ref
import Dovetail.Evaluate (ForeignType(..), builtIn)
    
type ST ctx a = Value ctx -> Eval ctx a

env :: forall ctx. Typeable ctx => Env ctx
env = do
  let _ModuleName = ModuleName "Control.Monad.ST.Internal"

  fold
    [ -- new :: forall a r. a -> ST r (STRef r a)
      builtIn @ctx @(Value ctx -> ST ctx (Ref ctx))
        _ModuleName "new"
        \s _ -> 
          ForeignType <$> liftIO (IORef.newIORef s)
      -- read :: forall a r. STRef r a -> ST r a
    , builtIn @ctx @(Ref ctx -> ST ctx (Value ctx))
        _ModuleName "read"
        \(ForeignType ref) _ ->
          liftIO (IORef.readIORef ref)
      -- write :: forall a r. a -> STRef r a -> ST r a
    , builtIn @ctx @(Value ctx -> Ref ctx -> ST ctx (Value ctx))
        _ModuleName "write"
        \s (ForeignType ref) _ -> do
          liftIO (IORef.writeIORef ref s)
          pure (Object mempty)
      -- modifyImpl :: forall r a b.           
      --   (a                    
      --    -> { state :: a      
      --       , value :: b      
      --       }                 
      --   )                     
      --   -> Ref r a -> ST r b
    , builtIn @ctx @((Value ctx -> Eval ctx (ModifyResult ctx)) -> Ref ctx -> ST ctx (Value ctx))
        _ModuleName "modifyImpl"
        \f (ForeignType ref) _ -> do
          s <- liftIO (IORef.readIORef ref)
          ModifyResult newState result <- f s
          liftIO (IORef.writeIORef ref newState)
          pure result
      -- map_ :: forall r a b. (a -> b) -> ST r a -> ST r b
    , builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> ST ctx (Value ctx) -> ST ctx (Value ctx))
        _ModuleName "map_"
        \f st rw ->
          f =<< st rw
      -- pure_ :: forall r a. a -> ST r a
    , builtIn @ctx @(Value ctx -> ST ctx (Value ctx))
        _ModuleName "pure_"
        \a _ -> pure a
      -- bind_ :: forall r a b. ST r a -> (a -> ST r b) -> ST r b
    , builtIn @ctx @(ST ctx (Value ctx) -> (Value ctx -> ST ctx (Value ctx)) -> ST ctx (Value ctx))
        _ModuleName "bind_"
        \st f rw ->
          st rw >>= \a -> f a rw
      -- run :: forall a. (forall r. ST r a) -> a
    , builtIn @ctx @(ST ctx (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "run"
        \st -> 
          st (Object mempty)
      -- while :: forall r a. ST r Boolean -> ST r a -> ST r Unit
    , builtIn @ctx @(ST ctx Bool -> ST ctx (Value ctx) -> ST ctx (Value ctx))
        _ModuleName "while"
        \cond body rw -> do
          let loop = do
                b <- cond rw
                when b (body rw *> loop)
          loop
          pure (Object mempty)
      -- for :: forall r a. Int -> Int -> (Int -> ST r a) -> ST r Unit
    , builtIn @ctx @(Integer -> Integer -> (Integer -> ST ctx (Value ctx)) -> ST ctx (Value ctx))
        _ModuleName "for"
        \from to body rw -> do
          for_ [from .. to] \i -> 
            body i rw
          pure (Object mempty)
      -- foreach :: forall r a. Array a -> (a -> ST r Unit) -> ST r Unit
    , builtIn @ctx @(Vector (Value ctx) -> (Value ctx -> ST ctx (Value ctx)) -> ST ctx (Value ctx))
        _ModuleName "foreach"
        \xs f rw -> do
          for_ xs \x -> 
            f x rw
          pure (Object mempty)
    ]
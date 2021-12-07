{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Control.Monad.ST.Internal where

import Control.Monad (when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (fold, for_)
import Data.IORef qualified as IORef
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Core.Effect.Ref
import Dovetail.Evaluate (ForeignType(..), builtIn)
    
type ST m a = Value m -> EvalT m a

env :: forall m. (MonadFix m, MonadIO m, Typeable m) => Env m
env = do
  let _ModuleName = ModuleName "Control.Monad.ST.Internal"

  fold
    [ -- new :: forall a r. a -> ST r (STRef r a)
      builtIn @m @(Value m -> ST m (Ref m))
        _ModuleName "new"
        \s _ -> 
          ForeignType <$> liftIO (IORef.newIORef s)
      -- read :: forall a r. STRef r a -> ST r a
    , builtIn @m @(Ref m -> ST m (Value m))
        _ModuleName "read"
        \(ForeignType ref) _ ->
          liftIO (IORef.readIORef ref)
      -- write :: forall a r. a -> STRef r a -> ST r a
    , builtIn @m @(Value m -> Ref m -> ST m (Value m))
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
    , builtIn @m @((Value m -> EvalT m (ModifyResult m)) -> Ref m -> ST m (Value m))
        _ModuleName "modifyImpl"
        \f (ForeignType ref) _ -> do
          s <- liftIO (IORef.readIORef ref)
          ModifyResult newState result <- f s
          liftIO (IORef.writeIORef ref newState)
          pure result
      -- map_ :: forall r a b. (a -> b) -> ST r a -> ST r b
    , builtIn @m @((Value m -> EvalT m (Value m)) -> ST m (Value m) -> ST m (Value m))
        _ModuleName "map_"
        \f st rw ->
          f =<< st rw
      -- pure_ :: forall r a. a -> ST r a
    , builtIn @m @(Value m -> ST m (Value m))
        _ModuleName "pure_"
        \a _ -> pure a
      -- bind_ :: forall r a b. ST r a -> (a -> ST r b) -> ST r b
    , builtIn @m @(ST m (Value m) -> (Value m -> ST m (Value m)) -> ST m (Value m))
        _ModuleName "bind_"
        \st f rw ->
          st rw >>= \a -> f a rw
      -- run :: forall a. (forall r. ST r a) -> a
    , builtIn @m @(ST m (Value m) -> EvalT m (Value m))
        _ModuleName "run"
        \st -> 
          st (Object mempty)
      -- while :: forall r a. ST r Boolean -> ST r a -> ST r Unit
    , builtIn @m @(ST m Bool -> ST m (Value m) -> ST m (Value m))
        _ModuleName "while"
        \cond body rw -> do
          let loop = do
                b <- cond rw
                when b (body rw *> loop)
          loop
          pure (Object mempty)
      -- for :: forall r a. Int -> Int -> (Int -> ST r a) -> ST r Unit
    , builtIn @m @(Integer -> Integer -> (Integer -> ST m (Value m)) -> ST m (Value m))
        _ModuleName "for"
        \from to body rw -> do
          for_ [from .. to] \i -> 
            body i rw
          pure (Object mempty)
      -- foreach :: forall r a. Array a -> (a -> ST r Unit) -> ST r Unit
    , builtIn @m @(Vector (Value m) -> (Value m -> ST m (Value m)) -> ST m (Value m))
        _ModuleName "foreach"
        \xs f rw -> do
          for_ xs \x -> 
            f x rw
          pure (Object mempty)
    ]
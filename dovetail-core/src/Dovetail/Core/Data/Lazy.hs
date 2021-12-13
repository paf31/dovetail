{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Lazy where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (fold)
import Data.IORef qualified as IORef
import Data.Typeable (Typeable)
import Dovetail
import Dovetail.Evaluate (ForeignType(..), builtIn)
import Language.PureScript qualified as P

type Lazy ctx a = ForeignType (Eval ctx a)

env :: forall ctx. Typeable ctx => Env ctx
env = do
  let _ModuleName = P.ModuleName "Data.Lazy"

  fold
    [ -- defer :: forall a. (Unit -> a) -> Lazy a
      builtIn @ctx @((Value ctx -> Eval ctx (Value ctx)) -> Eval ctx (Lazy ctx (Value ctx)))
        _ModuleName "defer"
        \f -> do
          ref <- liftIO (IORef.newIORef Nothing)
          pure . ForeignType $ do
            ma <- liftIO (IORef.readIORef ref)
            case ma of
              Nothing -> do
                val <- f (Object mempty)
                liftIO (IORef.writeIORef ref (Just val))
                pure val
              Just a -> pure a
      -- force :: forall a. Lazy a -> a
    , builtIn @ctx @(Lazy ctx (Value ctx) -> Eval ctx (Value ctx))
        _ModuleName "force"
        \(ForeignType f) -> f
    ]
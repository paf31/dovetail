{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Console where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Dovetail
import Dovetail.Core.Effect (Effect)
import Dovetail.Evaluate (builtIn)

env :: forall m. (MonadIO m, MonadIO m, Typeable m) => Env m
env = do
  let _ModuleName = ModuleName "Effect.Console"

      logImpl :: Text -> Effect m (Value m)
      logImpl s _ = do
        liftIO (Text.IO.putStrLn s)
        pure (Object mempty)
        
      notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

  fold
    [ -- log :: String -> Effect Unit
      builtIn @m @(Text -> Effect m (Value m))
        _ModuleName "log"
        logImpl
      -- warn :: String -> Effect Unit
    , builtIn @m @(Text -> Effect m (Value m))
        _ModuleName "warn"
        logImpl
      -- error :: String -> Effect Unit
    , builtIn @m @(Text -> Effect m (Value m))
        _ModuleName "error"
        logImpl
      -- info :: String -> Effect Unit
    , builtIn @m @(Text -> Effect m (Value m))
        _ModuleName "info"
        logImpl
      -- time :: String -> Effect Unit
    , builtIn @m @(Text -> Effect m (Value m))
        _ModuleName "time"
        \_ _ -> notImplemented "time"
      -- timeLog :: String -> Effect Unit
    , builtIn @m @(Text -> Effect m (Value m))
        _ModuleName "timeLog"
        \_ _ -> notImplemented "timeLog"
      -- timeEnd :: String -> Effect Unit
    , builtIn @m @(Text -> Effect m (Value m))
        _ModuleName "timeEnd"
        \_ _ -> notImplemented "timeEnd"
      -- clear :: Effect Unit
    , builtIn @m @(Text -> Effect m (Value m))
        _ModuleName "clear"
        \_ _ -> notImplemented "clear"
    ]

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Console where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Data.Typeable (Typeable)
import Dovetail
import Dovetail.Core.Effect (Effect)
import Dovetail.Evaluate (builtIn)

env :: forall ctx. Typeable ctx => Env ctx
env = do
  let _ModuleName = ModuleName "Effect.Console"

      logImpl :: Text -> Effect ctx (Value ctx)
      logImpl s _ = do
        liftIO (Text.IO.putStrLn s)
        pure (Object mempty)
        
      notImplemented :: Text -> Eval ctx a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

  fold
    [ -- log :: String -> Effect Unit
      builtIn @ctx @(Text -> Effect ctx (Value ctx))
        _ModuleName "log"
        logImpl
      -- warn :: String -> Effect Unit
    , builtIn @ctx @(Text -> Effect ctx (Value ctx))
        _ModuleName "warn"
        logImpl
      -- error :: String -> Effect Unit
    , builtIn @ctx @(Text -> Effect ctx (Value ctx))
        _ModuleName "error"
        logImpl
      -- info :: String -> Effect Unit
    , builtIn @ctx @(Text -> Effect ctx (Value ctx))
        _ModuleName "info"
        logImpl
      -- time :: String -> Effect Unit
    , builtIn @ctx @(Text -> Effect ctx (Value ctx))
        _ModuleName "time"
        \_ _ -> notImplemented "time"
      -- timeLog :: String -> Effect Unit
    , builtIn @ctx @(Text -> Effect ctx (Value ctx))
        _ModuleName "timeLog"
        \_ _ -> notImplemented "timeLog"
      -- timeEnd :: String -> Effect Unit
    , builtIn @ctx @(Text -> Effect ctx (Value ctx))
        _ModuleName "timeEnd"
        \_ _ -> notImplemented "timeEnd"
      -- clear :: Effect Unit
    , builtIn @ctx @(Text -> Effect ctx (Value ctx))
        _ModuleName "clear"
        \_ _ -> notImplemented "clear"
    ]

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Effect.Ref where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)


env :: forall m. MonadFix m => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

      _ModuleName = ModuleName "Effect.Ref"

  fold
    [
    ]

-- new :: forall s. s -> Effect (Ref s)
-- 
-- newWithSelf :: forall s. (Ref s -> s) -> Effect (Ref s)
-- 
-- read :: forall s. Ref s -> Effect s
-- 
-- modifyImpl :: forall s b.           
--   (s                  
--    -> { state :: s    
--       , value :: b    
--       }               
--   )                   
--   -> Ref s -> Effect b
-- 
-- write :: forall s. s -> Ref s -> Effect Unit

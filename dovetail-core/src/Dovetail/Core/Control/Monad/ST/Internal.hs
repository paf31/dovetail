{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Control.Monad.ST.Internal where

import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dovetail
import Dovetail.Evaluate (builtIn)
import Language.PureScript qualified as P

env :: forall m. MonadFix m => Env m
env = do
  let notImplemented :: Text -> EvalT m a
      notImplemented name = throwErrorWithContext (OtherError (name <> " is not implemented"))

      _ModuleName = P.ModuleName "Control.Monad.ST.Internal"

  fold
    [
    ]

-- map_ :: forall r a b. (a -> b) -> ST r a -> ST r b
-- 
-- pure_ :: forall r a. a -> ST r a
-- 
-- bind_ :: forall r a b. ST r a -> (a -> ST r b) -> ST r b
-- 
-- run :: forall a. (forall r. ST r a) -> a
-- 
-- while :: forall r a. ST r Boolean -> ST r a -> ST r Unit
-- 
-- for :: forall r a. Int -> Int -> (Int -> ST r a) -> ST r Unit
-- 
-- foreach :: forall r a. Array a -> (a -> ST r Unit) -> ST r Unit
-- 
-- new :: forall a r. a -> ST r (STRef r a)
-- 
-- read :: forall a r. STRef r a -> ST r a
-- 
-- modifyImpl :: forall r a b.           
--   (a                    
--    -> { state :: a      
--       , value :: b      
--       }                 
--   )                     
--   -> STRef r a -> ST r b
-- 
-- write :: forall a r. a -> STRef r a -> ST r a
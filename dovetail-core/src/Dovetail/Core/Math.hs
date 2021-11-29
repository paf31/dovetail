{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Math where

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

      _ModuleName = P.ModuleName "Math"

  fold
    [
    ]

-- abs :: Number -> Number
-- 
-- acos :: Number -> Radians
-- 
-- asin :: Number -> Radians
-- 
-- atan :: Number -> Radians
-- 
-- atan2 :: Number -> Number -> Radians
-- 
-- ceil :: Number -> Number
-- 
-- cos :: Radians -> Number
-- 
-- exp :: Number -> Number
-- 
-- floor :: Number -> Number
-- 
-- imul :: Int -> Int -> Int
-- 
-- log :: Number -> Number
-- 
-- max :: Number -> Number -> Number
-- 
-- min :: Number -> Number -> Number
-- 
-- pow :: Number -> Number -> Number
-- 
-- round :: Number -> Number
-- 
-- sin :: Radians -> Number
-- 
-- sqrt :: Number -> Number
-- 
-- tan :: Radians -> Number
-- 
-- trunc :: Number -> Number
-- 
-- remainder :: Number -> Number -> Number
-- 
-- e :: Number
-- 
-- ln2 :: Number
-- 
-- ln10 :: Number
-- 
-- log2e :: Number
-- 
-- log10e :: Number
-- 
-- pi :: Number
-- 
-- tau :: Number
-- 
-- sqrt1_2 :: Number
-- 
-- sqrt2 :: Number

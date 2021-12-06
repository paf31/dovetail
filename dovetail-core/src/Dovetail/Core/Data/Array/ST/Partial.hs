{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core.Data.Array.ST.Partial where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (fold)
import Data.Functor (($>))
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as Mutable
import Dovetail
import Dovetail.Core.Control.Monad.ST.Internal (ST)
import Dovetail.Core.Data.Array.ST (STArray)
import Dovetail.Evaluate (ForeignType(..), builtIn)
import Language.PureScript qualified as P

env :: forall m. (MonadFix m, MonadIO m, Typeable m) => Env m
env = do
  let _ModuleName = P.ModuleName "Data.Array.ST.Partial"

  fold
    [ -- peekImpl :: forall h a. Int -> STArray h a -> ST h a
      builtIn @m @(Integer -> STArray (Value m) -> ST m (Value m))
        _ModuleName "peekImpl"
        \i (ForeignType xs) _ ->
          if i >= 0 && fromIntegral i <= Mutable.length xs 
            then liftIO (Mutable.read xs (fromIntegral i))
            else throwErrorWithContext (OtherError "peekImpl: index out of range")
      -- poke :: forall h a. Int -> a -> STArray h a -> ST h Unit
    , builtIn @m @(Integer -> Value m -> STArray (Value m) -> ST m (Value m))
        _ModuleName "poke"
        \i x (ForeignType xs) _ ->
          if i >= 0 && fromIntegral i <= Mutable.length xs 
            then liftIO (Mutable.write xs (fromIntegral i) x) $> Object mempty
            else throwErrorWithContext (OtherError "poke: index out of range")
    ]
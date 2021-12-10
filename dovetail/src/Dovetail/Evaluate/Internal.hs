{-# LANGUAGE ImportQualifiedPost #-}

module Dovetail.Evaluate.Internal 
  ( Promise
  , emptyPromise
  , require
  , fulfill
  
  , EmptyPromise(..)
  ) where
  
import Control.Exception (Exception, throw)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

data EmptyPromise = EmptyPromise deriving Show

instance Exception EmptyPromise

data Promise a = Promise { getPromise :: IORef a }

emptyPromise :: IO (Promise a)
emptyPromise = Promise <$> IORef.newIORef (throw EmptyPromise)

require :: Promise a -> a
require (Promise r) = unsafePerformIO . unsafeInterleaveIO $ IORef.readIORef r

fulfill :: Promise a -> a -> IO ()
fulfill (Promise r) a = IORef.writeIORef r a
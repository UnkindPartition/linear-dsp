{-# LANGUAGE RecordWildCards #-}
module SlidingWindow.ST
  ( SlidingWindow
  , new
  , push
  , length
  , lookup
  , unsafeLookup
  , getPosition
  ) where

import Prelude hiding (length, lookup)
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Stack

data SlidingWindow s a = SlidingWindow
  { size :: !Int
  , buf :: !(MV.MVector s a)
  , position :: !Int
    -- ^ the count of the first sample in the sliding window, 1-based (0
    -- for the empty window)
  , startPtr :: !Int
    -- ^ the position of the first sample in the ring buffer
  , endPtr :: !Int -- ^ the position past the last sample in the ring buffer
  }

new
  :: MV.Unbox a
  => Int -- ^ size
  -> ST s (SlidingWindow s a)
new size = do
  buf <- MV.new size
  return SlidingWindow
    { position = 0
    , startPtr = 0
    , endPtr = 0
    , ..
    }

push
  :: MV.Unbox a
  => a
  -> SlidingWindow s a
  -> ST s (SlidingWindow s a)
push a sw@SlidingWindow{..} = do
  MV.unsafeWrite buf endPtr a
  return sw
    { position = position + 1
    , startPtr =
        if startPtr == endPtr
          then (startPtr + 1) `mod` size
          else startPtr
    , endPtr = (endPtr + 1) `mod` size
    }

length :: SlidingWindow s a -> Int
length SlidingWindow{..}
  | startPtr == endPtr = 0
  | otherwise = (endPtr - startPtr) `mod` size + 1

unsafeLookup
  :: (MV.Unbox a, HasCallStack)
  => Int
  -> SlidingWindow s a
  -> ST s a
unsafeLookup i SlidingWindow{..} = MV.unsafeRead buf ((startPtr + i) `mod` size)

lookup
  :: (MV.Unbox a, HasCallStack)
  => Int
  -> SlidingWindow s a
  -> ST s a
lookup i sw
  | i < 0 = error "lookup: negative index"
  | i >= length sw = error "lookup: index too large"
  | otherwise = unsafeLookup i sw

getPosition :: SlidingWindow s a -> Int
getPosition = position

{-# LANGUAGE LinearTypes, GADTs, RecordWildCards, NamedFieldPuns,
             ScopedTypeVariables #-}
module SlidingWindow.Linear
  ( SlidingWindow
  , withSlidingWindow
  , push
  , length
  , lookup
  , unsafeLookup
  )
  where

import Prelude hiding (length, lookup, read)
import Data.Unrestricted.Linear
import Data.Array.Mutable.Linear hiding (length)
import GHC.Stack

data SlidingWindowInfo = SlidingWindowInfo
  { size :: !Int
  , position :: !Int
    -- ^ the count of the first sample in the sliding window, 1-based (0
    -- for the empty window)
  , startPtr :: !Int
    -- ^ the position of the first sample in the ring buffer
  , endPtr :: !Int -- ^ the position past the last sample in the ring buffer
  }

data SlidingWindow a where
  SlidingWindow ::
    {-# UNPACK #-} !SlidingWindowInfo ->
    Array a #-> -- ring buffer
    SlidingWindow a

withSlidingWindow
  :: Num a
  => Int
  -> (SlidingWindow a #-> Unrestricted b)
  -> Unrestricted b
withSlidingWindow size k =
  alloc size 0 $ \buf ->
    k (SlidingWindow (SlidingWindowInfo
      { size
      , position = 0
      , startPtr = 0
      , endPtr = 0 })
      buf)

push ::
  a ->
  SlidingWindow a #->
  SlidingWindow a
push a (SlidingWindow si@SlidingWindowInfo{..} buf) =
  SlidingWindow
    si
      { position = position+1
      , startPtr =
          if startPtr == endPtr
            then (startPtr + 1) `mod` size
            else startPtr
      , endPtr = (endPtr + 1) `mod` size
      }
    (write buf endPtr a)

length ::
  SlidingWindow a #->
  (SlidingWindow a, Int)
length (SlidingWindow si@SlidingWindowInfo{..} buf) =
  let
    len
      | startPtr == endPtr = 0
      | otherwise = (endPtr - startPtr) `mod` size + 1
  in (SlidingWindow si buf, len)

lookup :: HasCallStack =>
  Int ->
  SlidingWindow a #->
  (SlidingWindow a, a)
lookup i sw
  | i < 0 = error "lookup: negative index"
  | i >= snd (length sw) = error "lookup: index too large"
  | otherwise = unsafeLookup i sw

unsafeLookup :: forall a . HasCallStack =>
  Int ->
  SlidingWindow a #->
  (SlidingWindow a, a)
unsafeLookup i (SlidingWindow si@SlidingWindowInfo{..} buf) =
  k (read buf ((startPtr + i) `mod` size))
  where
    k :: (Array a, a) #-> (SlidingWindow a, a)
    k (buf', a) = (SlidingWindow si buf', a)

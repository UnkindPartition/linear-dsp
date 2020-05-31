{-# LANGUAGE LinearTypes, GADTs, RecordWildCards, NamedFieldPuns #-}
module SlidingWindow.Linear
  ( SlidingWindow
  , withSlidingWindow
  , push
  )
  where

import Data.Unrestricted.Linear
import Data.Array.Mutable.Linear

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

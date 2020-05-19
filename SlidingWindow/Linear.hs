{-# LANGUAGE LinearTypes, GADTs #-}
module SlidingWindow.Linear
  ( SlidingWindow
  , withSlidingWindow
  , push
  )
  where

import Data.Unrestricted.Linear
import Data.Array.Mutable.Linear

data SlidingWindow a where
  SlidingWindow ::
    !Int -> -- size
    Array a #-> -- ring buffer
    !Int -> -- position
    !Int -> -- start ptr
    !Int -> -- end ptr
    SlidingWindow a

withSlidingWindow
  :: Num a
  => Int
  -> (SlidingWindow a #-> Unrestricted b)
  -> Unrestricted b
withSlidingWindow size k =
  alloc size 0 $ \buf ->
    k (SlidingWindow size buf 0 0 0)

push ::
  a ->
  SlidingWindow a #->
  SlidingWindow a
push a (SlidingWindow size buf position startPtr endPtr) =
  SlidingWindow size (write buf endPtr a) (position+1) startPtr' endPtr'
  where
    startPtr' =
      if startPtr == endPtr
        then (startPtr + 1) `mod` size
        else startPtr
    endPtr' = (endPtr + 1) `mod` size

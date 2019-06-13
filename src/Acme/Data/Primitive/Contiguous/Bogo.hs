{-# language BangPatterns #-}
{-# language ForeignFunctionInterface #-}

-- | Bogosort contiguous datastructures.
--   To make matters worse, we operate on lists,
--   and turn those lists back into the original structure.
module Acme.Data.Primitive.Contiguous.Bogo
  ( bogo
  ) where

import Prelude hiding (read)

import Control.Applicative (liftA2)
import Control.Monad (void, unless, (>=>))
import Control.Monad.Primitive (PrimMonad(..))
import Data.Primitive.Contiguous
import GHC.Exts (RealWorld)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Primitive.Contiguous as C

sorted :: (Contiguous arr, Element arr a, Ord a, PrimMonad m)
  => Mutable arr (PrimState m) a
  -> m Bool
sorted !marr = do
  !sz <- sizeMutable marr
  let go !ix = if ix < 2
        then pure True
        else do
          b <- liftA2 (<) (read marr (ix - 1)) (read marr (ix - 2))
          if b then pure False else go (ix - 1)
  go sz

bogo :: (Contiguous arr, Element arr a, Ord a)
  => arr a
  -> arr a
bogo arr = unsafePerformIO $ do
  let sz = C.size arr
  marr <- C.new sz
  C.copy marr 0 arr 0 sz
  bogoMutable marr
  C.unsafeFreeze marr

bogoMutable :: (Contiguous arr, Element arr a, Ord a)
  => Mutable arr RealWorld a
  -> IO ()
bogoMutable marr = do
  let go = do
        isSorted <- sorted marr
        unless isSorted $ do
          randomise marr
          go
  go

randomise :: (Contiguous arr, Element arr a)
  => Mutable arr RealWorld a
  -> IO ()
randomise !marr = do
  srand
  !sz <- sizeMutable marr
  let go !ix = if ix > 0
        then do
          j <- (`mod` (ix + 1)) <$> c_rand
          swap marr ix j
          go (ix - 1)
        else pure ()
  go (sz - 1)
{-# inline randomise #-}

foreign import ccall "srand" c_srand :: Int -> IO Int
foreign import ccall "time" c_time :: Int -> IO Int
foreign import ccall "rand" c_rand :: IO Int

srand :: IO ()
srand = void $ (c_time >=> c_srand) 0

-- | Swap the elements of the mutable array at the given indices.
swap :: (Contiguous arr, Element arr a, PrimMonad m)
  => Mutable arr (PrimState m) a
  -> Int
  -> Int
  -> m ()
swap !marr !ix1 !ix2 = do
  atIx1 <- read marr ix1
  atIx2 <- read marr ix2
  write marr ix1 atIx2
  write marr ix2 atIx1
{-# inline swap #-}

-- | Bogosort contiguous datastructures.
--   To make matters worse, we operate on lists,
--   and turn those lists back into the original structure.
module Acme.Data.Primitive.Contiguous.Bogo
  ( bogo
  ) where

import Data.Primitive.Contiguous
import GHC.Exts (RealWorld)
import System.Random.Shuffle (shuffleM)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Foldable as F
import qualified Data.Primitive.Contiguous as C

data Ordered a = Empty | Decreasing a | Increasing a

inc :: Ordered a -> Bool
inc (Decreasing _) = False
inc _              = True

increasing :: (Foldable t, Ord a) => t a -> Bool
increasing = inc . F.foldl' go Empty where
  go Empty y = Decreasing y
  go (Increasing x) _ = Increasing x
  go (Decreasing x) y
    | x >= y = Decreasing y
    | otherwise = Increasing y

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
  let go xs = if increasing xs
        then pure ()
        else shuffleM xs >>= go
  marrList <- C.toListMutable marr
  go marrList

-- This is one of the code jam problems I've had the most difficulty
-- getting to pass without TLE in Haskell. Ever.

{-# LANGUAGE MagicHash #-}

module Main (main) where

import Control.Monad
import Data.Semigroup
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import qualified Data.ByteString.Char8 as B

-- Integer logarithm primop.
-- I don't think this one makes or breaks performance, but now I've
-- added it I might as well keep it.
import GHC.Integer.Logarithms
import GHC.Prim
import GHC.Types
log2 :: Int -> Int
log2 (I# e#) = I# (wordLog2# (int2Word# e#))

readInt :: B.ByteString -> Int
readInt bs = let Just (i,_) = B.readInt bs in i

main :: IO ()
main = do
  t <- readInt <$> B.getLine
  forM_ [1..t] $ \i -> do
    [n,k] <- map readInt . B.words <$> B.getLine
    cs <- listArray (1,n) . map readInt . B.words <$> B.getLine
    ds <- listArray (1,n) . map readInt . B.words <$> B.getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve n k cs ds)

solve :: Int -> Int -> UArray Int Int -> UArray Int Int -> Int
solve n k cs ds = sum [ fair i | i <- [1..n] ] where
  cRmq = rmqFrom cs; dRmq = rmqFrom ds

  -- | Number of intervals resulting in a fair fight if Charles picks
  -- sword i.
  fair :: Int -> Int
  fair i = intCount i (intIntersect ciBest ciGood)
           - intCount i (intIntersect ciBest dBad)
    where
      ciBest = i `isBest` cRmq
      ciGood = (i `hasMaxUnder` ((cs!i)+k)) dRmq
      dBad = (i `hasMaxUnder` ((cs!i)-k-1)) dRmq

  -- | Largest interval containing i where i is the leftmost maximum.
  isBest :: Int -> RMQ -> Interval
  isBest i r = I
    (bsearch (\a -> let Max (Arg _ i') = rmq r a i in i' == i) 0 i)
    (bsearch' (\b -> let Max (Arg _ i') = rmq r i b in i' == i) i (n+1))

  -- | Largest interval containing i whose max is beneath v.
  hasMaxUnder :: Int -> Int -> RMQ -> Interval
  hasMaxUnder i v r = I
    (bsearch (\a -> rmq r a i <= Max (Arg v undefined)) 0 (i+1))
    (bsearch' (\b -> rmq r i b <= Max (Arg v undefined)) i (n+1))

{-# INLINE bsearch #-}
bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch p = go where
  go a b | m <= a = b
         | p m = go a m
         | otherwise = go m b
    where m = (a + b) `div` 2

{-# INLINE bsearch' #-}
bsearch' :: (Int -> Bool) -> Int -> Int -> Int
bsearch' p = go where
  go a b | m >= b = a
         | p m = go m b
         | otherwise = go a m
    where m = (a + b + 1) `div` 2

data RMQ = RMQ !(UArray (Int,Int) Int) !(UArray (Int,Int) Int)

rmqFrom :: UArray Int Int -> RMQ
rmqFrom a = runST $ do
  let (l,h) = bounds a
      n = log2 (h-l+1)
  b <- newArray_ ((l,0),(h,n)) :: ST s (STUArray s (Int,Int) Int)
  arg <- newArray_ ((l,0),(h,n)) :: ST s (STUArray s (Int,Int) Int)
  forM_ [0..n] $ \j -> do
    forM_ [l..h] $ \i -> do
      if j == 0
      then do
        writeArray b (i,j) (a!i)
        writeArray arg (i,j) i
      else do
        left <- readArray b (i,j-1)
        leftArg <- readArray arg (i,j-1)
        right <- readArray b (min h (i+2^(j-1)),j-1)
        rightArg <- readArray arg (min h (i+2^(j-1)),j-1)
        let Max (Arg u v) = Max (Arg left leftArg) <> Max (Arg right rightArg)
        writeArray b (i,j) u
        writeArray arg (i,j) v
  RMQ <$> freeze b <*> freeze arg

{-# INLINE rmq #-}
rmq :: RMQ -> Int -> Int -> Max (Arg Int Int)
rmq (RMQ m arg) l r = left <> right
  where j = log2 (r - l + 1)
        left = Max (Arg (m ! (l,j)) (arg ! (l,j)))
        right = Max (Arg (m ! (r-2^j+1,j)) (arg ! (r-2^j+1,j)))

data Interval = I !Int !Int

intCount :: Int -> Interval -> Int
intCount i (I a b) = (i - a + 1) * (b - i + 1)

intIntersect :: Interval -> Interval -> Interval
intIntersect (I a b) (I c d) = I (max a c) (min b d)

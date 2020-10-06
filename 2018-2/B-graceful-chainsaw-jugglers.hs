module Main(main,brute,small,medium,large) where

import Data.Array.Unboxed
import Data.Array.ST
import Data.List
import Control.Arrow
import Control.Monad
import Control.Monad.ST

main :: IO ()
main = interact $ unlines . zipWith format [1..] .
                  map (solve . map read . words) .
                  tail . lines
  where format i j = "Case #" ++ show i ++ ": " ++ show j
        solve [r,b] = large ! (r,b)

-- brute: try powerset of possible (R,B) pairs --- O(2^(N^2))
brute :: UArray (Int,Int) Int
brute = runSTUArray $ do
  let sum' = foldl' (uncurry (***) . ((+) *** (+))) (0,0)
      bs = ((0,0),(4,4))
  dp <- newArray bs 0
  forM_ (filterM (const [False,True]) (range bs)) $ \rbs -> do
    let s = sum' rbs
    when (inRange bs s) $ writeArray dp s (length rbs)
  pure dp

-- DP on (using only pairs <= i,R,B) --- O(R^2×B^2) time and space
small :: UArray (Int,Int) Int
small = runSTUArray $ do
  let upTo = 50
      gb = ((0,0),(upTo,upTo))
      iMax = rangeSize gb - 1
      bs = ((0,(0,0)),(iMax,(upTo,upTo)))
  dp <- newArray bs minBound :: ST s (STUArray s (Int,(Int,Int)) Int)
  writeArray dp (0,(0,0)) 0
  forM_ (range ((1,(0,0)),(iMax,(upTo,upTo)))) $ \p@(i,(r,b)) -> do
    prev <- readArray dp (i-1,(r,b))
    let (ri,bi) = i `divMod` (upTo + 1)
        p' = (i-1,(r-ri,b-bi))
    cur <- fmap (max prev . maybe minBound (+1)) $
           traverse (readArray dp) $ mfilter (inRange bs) (Just p')
    writeArray dp p cur
  newListArray ((0,0),(upTo,upTo)) =<<
    mapM (readArray dp) [ (iMax,p) | p <- range gb ]

-- same DP, same time, less space O(RB)
medium :: UArray (Int,Int) Int
medium = runSTUArray $ do
  let upTo = 50
      bs = ((0,0),(upTo,upTo))
      rg = reverse $ tail $ range bs
  dp <- newArray bs minBound
  writeArray dp (0,0) 0
  forM_ rg $ \(ri,bi) -> forM_ rg $ \(r,b) -> do
    v <- readArray dp (r,b)
    let p' = (r-ri,b-bi)
    v' <- fmap (max v . maybe minBound (+1)) $
          traverse (readArray dp) $ mfilter (inRange bs) (Just p')
    writeArray dp (r,b) v'
  pure dp

-- DP on (much fewer pairs <= i,R,B) --- O(R^4/3×B^4/3)
large :: UArray (Int,Int) Int
large = runSTUArray $ do
  let upTo = 500
      bs = ((0,0),(upTo,upTo))
      rg = tail [ (i,j) | (i,j) <- range bs, (j+1)*i*(i+1) `div` 2 <= upTo
                                           , (i+1)*j*(j+1) `div` 2 <= upTo ]
  dp <- newArray bs 0
  forM_ rg $ \(ri,bi) -> do
    forM_ (reverse (range bs)) $ \(r,b) -> do
      v <- readArray dp (r,b)
      let p' = (r-ri,b-bi)
      v' <- fmap (max v . maybe minBound (+1)) $
            traverse (readArray dp) $ mfilter (inRange bs) (Just p')
      writeArray dp (r,b) v'
  pure dp

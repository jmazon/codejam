{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Array
import Data.Ratio
import Data.List (sortBy)
import Data.Function
import Control.Applicative
import Control.Monad
import System.IO

-- import Debug.Trace
-- tr a = traceShow a a
tr = id
traceShow = const id

type NTargets = Int
type RopeLength = Double
type Target = (Int,Int)

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM [1..t] $ \t -> do
    (n,r,ts) <- parseInput
    putStrLn $ "Case #" ++ show t ++ ": " ++ show (solve n r ts)

parseInput :: IO (Int,Int,[(Int,Int)])
parseInput = do
  [n,r] <- map read . words <$> getLine
  ts <- replicateM n $ do
    [x,y] <- map read . words <$> getLine
    return (x,y)
  return (n,r,ts)

--sample1 :: (NTargets,RopeLength,[Target])
sample1 = (6,24,[(0,0),(3,1),(12,4),(14,5),(13,7),(7,10)])
sample4 = (2,10,[(0,0),(4,0)])
sample5 = (3,50,[(0,0),(9,0),(10,0)])

solve n r ts = b
    where a = listArray (0,n-1) ts
          (b,p) = solve' a (fromIntegral r) (-1,1) (0,0)

solve' a r m n | traceShow (r,m,n) False = undefined
               | null ps   = tr (0, [])
               | otherwise = tr $
                             max (bendsWith + bendWithCounts, p:pathWith)
                                 (bendsWithout,pathWithout)
    where ps  = sortBy cmp $
                filter ((< r - 0.5) . dist n) $
                filter rightSide $
                filter (/= n) $
                elems a
          cmp a b | sin a == 0 = 0 `compare` cos a
                  | sin b == 0 = cos b `compare` 0
                  | otherwise  = cotan b `compare` cotan a
          cotan o = cos o % sin o
          cos o = scalar (n /-/ m) (o /-/ n)
          sin o = cross (n /-/ m) (o /-/ n)
          rightSide o = sin o >= 0
          p = head ps
          bendWithCounts = if sin p /= 0 || cos p < 0 then 1 else 0
          (bendsWith,pathWith) = solve' a (r - dist p n) n p
          (bendsWithout,pathWithout) = solve' a (dist p n) m n
          

(x1,y1) /-/ (x2,y2) = (x1-x2,y1-y2)
scalar (x1,y1) (x2,y2) = x1*x2 + y1*y2
cross (x1,y1) (x2,y2) = x1*y2 - x2*y1
dist m n = sqrt . fromIntegral $ scalar r r
    where r = n /-/ m

epsilon = 2 ** (-19) -- I hate it when it boils to the right choice of that
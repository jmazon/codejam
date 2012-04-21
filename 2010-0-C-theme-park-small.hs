{-# LANGUAGE BangPatterns #-}

import Control.Monad.Reader
import Data.Array
import Data.Functor

readIntLine = fmap read . words <$> getLine

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \tt -> do
    [r, k, n] <- readIntLine
    gs <- listArray (0,n-1) <$> readIntLine
    let park = Park r k n gs
    putStrLn $ "Case #" ++ show tt ++ ": "
               ++ show (dayIncome park)

type Group = Int     -- actually an index
type GroupSize = Int -- enough in both small and large sets
type GroupList = Array Group GroupSize

data Park = Park { runs :: Int
                 , capacity :: GroupSize
                 , groupCount :: Group
                 , groups :: GroupList }

boardCoaster (i,r) = capacity <$> ask >>= boardCoaster' (i,r) i
boardCoaster' (!i,!r) i0 !c = do
  s <- groupSize i
  i'<- nextGroup i
  c0 <- capacity <$> ask
  if c >= s &&             -- coaster full
     (i /= i0 || c == c0)  -- queue exhausted
    then boardCoaster' (i',r + fromIntegral s) i0 (c-s)
    else return (i,r)

groupSize i = groups <$> ask >>= return . (!i)
nextGroup i = groupCount <$> ask >>= return . mod (succ i)

dayIncome = do
  r <- runs <$> ask
  snd <$> foldM (const . boardCoaster) (0,0) [1..r]

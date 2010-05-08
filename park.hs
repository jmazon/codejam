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

data Coaster = Coaster { firstGroup :: Group
                       , income :: Integer }

boardCoaster c = capacity <$> ask >>= boardCoaster' c (firstGroup c)
boardCoaster' c f0 !r = do
  let f = firstGroup c
      i = income c
  s <- groupSize f
  f'<- nextGroup f
  r0 <- capacity <$> ask
  if r >= s &&             -- coaster full
     (f /= f0 || r == r0)  -- queue exhausted
    then boardCoaster' (Coaster f' (i + fromIntegral s)) f0 (r-s)
    else return c

groupSize i = groups <$> ask >>= return . (!i)
nextGroup i = groupCount <$> ask >>= return . mod (succ i)

dayIncome = do
  r <- runs <$> ask
  income <$> foldM (const . boardCoaster) (Coaster 0 0) [1..r]

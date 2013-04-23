-- Let it be noted that I really wanted to do this one in J,
-- but couldn't get the I/O done properly in time.

import Data.List (foldl1')
import Control.Monad (liftM,forM_,replicateM)
getInts = liftM (map read . words) getLine
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [n,m] <- getInts
    l <- replicateM n getInts
    putStrLn $ "Case #" ++ show i ++ ": " ++ if (solve l) then "YES" else "NO"
solve l = l == l'
    where h = map maximum l
          v = foldl1' (zipWith max) l
          l' = map (flip map v . min) h

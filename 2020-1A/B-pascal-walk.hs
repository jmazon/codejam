import Control.Monad
import Data.Bits
main = do
  t <- readLn
  forM [1..t] $ \i -> do
    putStrLn $ "Case #" ++ show i ++ ":"
    n <- readLn
    forM_ (walk n) $ \(r,k) -> putStrLn $ show r ++ " " ++ show k
walk n | n < 30 = [ (i,1) | i <- [1..n] ]
       | otherwise = go 0 (cycle [(const 1,id),(id,reverse)]) n where
  go b pds@((p,d):pds') r
    | b == 30            = [ (i,p i) | i <- [31..31+r-1] ]
    | (n-30) `testBit` b = d [ (b+1,i) | i <- [1..b+1] ] ++ go (b+1) pds' (r-2^b)
    | otherwise          = (b+1,p (b+1)) : go (b+1) pds (r-1)

import Data.MemoCombinators
import Control.Monad
import Control.Applicative
import System.IO

brute n = length $ filter isPure subsets
    where subsets = filterM (const [False,True]) [2..n]
          isPure s = isPure' s 1
          isPure' s k | k > length s   = False
                      | i == n = True
                      | otherwise       = isPure' s i
              where i = s !! (k-1)

-- A007059
f :: Int -> Int -> Int
f   n  _ | n < 0 = 0
f   0  _ = 1
f   _  0 = 0
f   n  m = sum [ f' (n-i) m | i <- [1..m] ] `mod` 100003

f' = memo2 bits bits f

fine n = sum [ f' (n-1-i) i | i <- [0..n] ] `mod` 100003

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    n <- read <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (fine n)

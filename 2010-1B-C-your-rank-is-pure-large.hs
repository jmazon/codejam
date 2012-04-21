import Data.Array
import Control.Monad
import Control.Applicative
import System.IO

f :: Int -> Int -> Int
f   n  m = sum [ f' (n-i) m | i <- [1..m] ] `mod` 100003

f'   n  _ | n < 0 = 0
f'   0  _ = 1
f'   _  0 = 0
f' n m = c ! (n,m)

c = array ((0,0),(500,500)) [ ((n,m), f n m) | n <- [0..500], m <- [0..500] ]

fine n = sum [ f' (n-1-i) i | i <- [0..n] ] `mod` 100003

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    n <- read <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (fine n)

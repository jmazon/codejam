import Data.Functor
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    p <- read <$> getLine
    ms <- map read . words <$> getLine
    replicateM_ p getLine -- ignore prices
    putStrLn $ "Case #" ++ show t ++ ": " ++ show (solve ms)

solve ms = solve' ms 0
solve' [] a = a
solve' ms a = solve' ms' $! a+n
    where (n,ms') = turn ms

turn ms = n `seq` (n, ms')
    where ps = pairs ms
          n = length $ filter shouldBuyMatch ps
          ms' = map newWish ps

shouldBuyMatch (0,_) = True
shouldBuyMatch (_,0) = True
shouldBuyMatch   _   = False

newWish (x,y) = min x' y'
    where x' = max (x-1) 0
          y' = max (y-1) 0

pairs (x:y:xs) = (x,y) : pairs xs
pairs _ = []

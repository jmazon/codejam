import Data.List (foldl')
import Control.Monad (forM_)

solve a b = sum $ map rec [a..b]
    where w = width a
          rec n = length $ filter (<= b) $ filter (> n) $ recycle w n

rotate n x = 10^n * r + q where (q,r) = x `divMod` 10
recycle n x = x : takeWhile (/= x) (tail $ iterate (rotate n) x)

width n | n >= 1000000 = 6
        | n >=  100000 = 5
        | n >=   10000 = 4
        | n >=    1000 = 3
        | n >=     100 = 2
        | n >=      10 = 1
        | otherwise    = 0

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [a,b] <- fmap (map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve a b)

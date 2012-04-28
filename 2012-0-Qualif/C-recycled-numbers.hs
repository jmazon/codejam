import Data.List (foldl')
import qualified Data.IntSet as S
import Control.Monad (forM_)
import System.IO

solve a b = snd $ foldl' go (S.empty,0) [a..b]
    where n = width a
          go (s,t) x | x `S.member` s = (s,t)
                     | otherwise      = s' `seq` t' `seq` (s',t')
              where c = filter (<= b) $ filter (>= a) $ recycle n x
                    l = length c
                    s' = foldl' (flip S.insert) s c
                    t' = t + l * (l-1) `div` 2

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
  hSetBuffering stdout LineBuffering
  t <- readLn
  forM_ [1..t] $ \i -> do
    [a,b] <- fmap (map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve a b)

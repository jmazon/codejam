import Control.Monad (forM_)
import Data.List (sort,sortOn)
import Data.Ord (Down(Down))

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [_,k] <- map read . words <$> getLine
    ps <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve k (sort ps))

solve :: Int -> [Int] -> Double
solve k ps =
  fromIntegral (max (sum $ take 2 $ sortOn Down singles) (maximum doubles))
  / fromIntegral k
  where
    singles = atStart : atEnd : map single rs
    atStart = head ps - 1
    atEnd = k - last ps
    doubles = 0 : map double rs
    single (a,b) = 1 + (b-a) `div` 2
    double (a,b) = b - a + 1
    rs = ranges ps
    ranges (x:y:zs) | x+1 <= y-1 = (x+1,y-1) : ranges (y:zs)
                    | otherwise =             ranges (y:zs)
    ranges _ = []

import Control.Monad (forM_)
import Data.List (sort,sortOn)
import Data.Ord (Down(Down))

solve :: Int -> [Int] -> Double
solve k ps =
  let
    single l r = (r - l) `div` 2
    atStart = head ps - 1
    atEnd = k - last ps
    singles = atStart : atEnd : zipWith single ps (tail ps)
    double l r = r - l - 1
    doubles = zipWith double ps (tail ps)
  in
    fromIntegral (max (maximum (0 : doubles))
                      (sum $ take 2 $ sortOn Down singles))
    / fromIntegral k

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [_,k] <- map read . words <$> getLine
    ps <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve k (sort ps))

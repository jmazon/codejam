import Control.Monad
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [a,b] <- fmap (map read . words) getLine
    as <- fmap (map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve a b as)

solve :: Int -> Int -> [Double] -> Double
solve a l = minimum .
            (fromIntegral (l + 2) :) .
            zipWith f [a,a-1..] .
            scanl (*) 1
    where f b p =    p  * fromIntegral (b + b + l-a + 1) +
                  (1-p) * fromIntegral (b + b + l-a + 1 + l + 1)

import Data.List
import Control.Monad
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [a,n] <- fmap (map read . words) getLine
    ns <- fmap (map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve a (sort ns))
solve _ [] = 0 :: Int
solve a (n:ns)
    | a > n     = solve (a+n) ns
    | otherwise = min (1 + solve a ns)
                      (if a > 1 then 1 + solve (2*a - 1) (n:ns) else maxBound)

import Data.List
import Control.Monad
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    ls <- replicateM n (map read . words <$> getLine)
    let k = sum $ zipWith (!!) ls [0..]
        r = length $ filter (\r -> nub r /= r) ls
        c = length $ filter (\c -> nub c /= c) (transpose ls)
    putStrLn $ "Case #" ++ show i ++ ": " ++ unwords [show k,show r,show c]

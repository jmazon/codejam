import Data.List
import Control.Monad
import Control.Arrow

main = do
  n <- readLn
  forM_ [1..n] $ \i -> do
    [l,w] <- (map read . words) `liftM` getLine
    m <- replicateM w getLine
    let s = transpose $ map (snd . mapAccumL f 1) m
        f n c | c `elem` "GS" = (n+1, n)
              | otherwise     = (1, 0)
        a = maximum $ concatMap (map (uncurry (*) . (length &&& minimum)) . filter (not . null) . concatMap tails . inits) s
    putStrLn $ "Case #" ++ show i ++ ": " ++ show a
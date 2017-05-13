import Data.List
import Control.Monad
import Control.Arrow

main = do
  t <- readLn
  forM_ [1..t] $ \tn -> do
    [n,k] <- map read . words  <$> getLine :: IO [Int]
    u <- readLn
    ps <- map (head &&& length) . group . sort . map read . words <$> getLine
    let go :: [(Double,Int)] -> Double -> [(Double,Int)]
        go ps u | abs u < 1e-12 = ps
        go ((p1,n1):ps@((p2,n2):ps')) u
          | u < delta = (p1 + u/fromIntegral n1,n1) : ps
          | otherwise = go ((p2,n1+n2):ps') (u - delta)
          where delta = fromIntegral n1 * (p2 - p1)
        comp = product . map (uncurry (^))
    putStrLn $ "Case #" ++ show tn ++ ": " ++ show (comp $ go (ps ++ [(1,1)]) u)

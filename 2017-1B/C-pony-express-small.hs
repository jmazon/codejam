import Data.Array    (listArray,(!))
import Control.Monad (forM_,replicateM)
readHorse l = (read e,read s) where [e,s] = words l
main = do
  t <- readLn
  forM_ [1..t] $ \tn -> do
    [n,q@1] <- map read . words <$> getLine
    hs0 <- listArray (1,n) . map readHorse <$> replicateM n getLine
    ds <- listArray (1,n) . scanl (+) 0 .
          map read . flip (zipWith (!!)) [1..] <$>
          replicateM n (words <$> getLine)
    replicateM q getLine
    let dp = let a = listArray (1,n) (dp' <$> [1..n]) in \i -> a!i
        dp' i | i == n = 0
              | otherwise = minimum [ fromIntegral (ds!j-ds!i) / s + dp j
                                    | let (e,s) = hs0 ! i
                                    , j <- takeWhile ((<= ds!i + e) . (ds!))
                                                     [i+1..n] ]
    putStrLn $ "Case #" ++ show tn ++ ": " ++ show (dp 1)

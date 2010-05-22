import Data.Array
import Control.Applicative
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \j -> do
    [d,i,m,n] <- map read . words <$> getLine
    ns <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show j ++ ": " ++ show (solve d i m n ns)

--solve :: Int -> Int -> Int -> Int -> [Int] -> Int
solve d i m n ns = minimum [ s n f | f <- [0..255] ]
    where a = listArray (1,n) ns
          c = listArray ((0,0),(n,255))
              [ s' n' f | n' <- [0..n], f <- [0..255] ]
          s n f = c ! (n,f)
          s' 0 f             = 0
          s' n f | m > 0     = minimum $ s (n-1) f + d :
                               [ s (n-1) p +
                                 abs (f - a!n) +
                                 i * max 0 ((abs (f - p) - 1) `div` m)
                                 | p <- [0..255] ]
                 | otherwise = s (n-1) f + min d (abs (f - a!n))

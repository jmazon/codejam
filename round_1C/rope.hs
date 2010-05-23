import Data.List
import Control.Applicative
import Control.Monad
import Control.Arrow
import System.IO


main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    test <- readTest
    putStrLn $ "Case #" ++ show t ++ ": " ++ show (solve test)

readTest = do
  n <- read <$> getLine
  replicateM n ( ((!!0) &&& (!!1)) . map read . words <$> getLine )

solve :: [(Int,Int)] -> Int
solve t = foldl' f 0 t'
    where t' = sort t
          f n w@(i,j) = n + crosses j (takeWhile (/= w) t') 0
          crosses _ [] a = a
          crosses j ((_,k):ws) a = a' `seq` crosses j ws a'
              where a' = a + if k > j then 1 else 0

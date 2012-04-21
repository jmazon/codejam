import Data.Maybe
import Data.Array
import System.IO
import Control.Monad
import Control.Applicative

compatible a b = compatible' $ zipWith compare a b
compatible' [] = True
compatible' (EQ:_) = False
compatible' (LT:GT:_) = False
compatible' (GT:LT:_) = False
compatible' (_:xs) = compatible' xs


main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    [n,k] <- map read . words <$> getLine
    ss <- map (map read . words) <$> replicateM n getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve n ss)

solve n ss = head . catMaybes . solve' a n $ [1..]
    where a = listArray (1,n) [ (i, [ j | j <- ss, compatible i j ]) | i <- ss ]

solve' a n k = solve'' a n $ replicate k []

solve'' a n cs = 
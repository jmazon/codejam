import Data.Functor
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    [k,c] <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show t ++ ": " ++ show (solve k c)

solve k c = solve' k c 0 0
solve' k c n s | s >= k*c  = n
               | otherwise = n' `seq` s' `seq` solve' k c n' s'
    where n' = n + 1
          s' = s + 1 + s `div` k

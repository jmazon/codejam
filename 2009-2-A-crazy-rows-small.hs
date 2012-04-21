import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List

solve _ [] = 0
solve n xs = r + solve (n+1) (delete m xs)
    where m = fromJust $ find (<= n) xs
          r = fromJust $ elemIndex m xs

stringToN s = let l = filter ((=='1') . fst) $ zip s [1..]
              in if null l then 0 else snd . last $ l

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    n <- read <$> getLine
    ss <- map stringToN <$> replicateM n getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve 1 ss)
import Data.List
import Data.Array.Unboxed
import Data.Maybe
import Control.Monad
import Text.Printf

needle = "welcome to code jam"
needle' = zip [0..] needle

s1 = "elcomew elcome to code jam"
s2 = "wweellccoommee to code qps jam"
s3 = "welcome to codejam"

start :: Array Int Int
start = accumArray (+) 0 (0, length needle) [(0,1)]

count s = (foldl' letter start s) ! (length needle)

letter a l = a // (letter' a l)

letter' a l = catMaybes $ map f needle'
    where f (i, n) | n == l    = Just (i', (a!i + a!i') `mod` 10000)
                   | otherwise = Nothing
              where i' = succ i

main = do
  n <- read `liftM` getLine
  forM_ [1..n] $ \i -> do
         haystack <- getLine
         printf "Case #%d: %04d\n" (i::Int) (count haystack)

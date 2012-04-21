import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    input <- getLine
    let output = eval input
    putStrLn $ "Case #" ++ show i ++ ": " ++ show output

eval :: String -> Integer
eval n = foldl c 0 n
    where base = swap01 . nub $ n
          b = max (genericLength base) 2
          c :: Integer -> Char -> Integer
          c n d = n*b + fromIntegral (fromJust $ elemIndex d base)

swap01 (a:b:r) = b:a:r
swap01 r = "*" ++ r
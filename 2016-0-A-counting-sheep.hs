import Data.Array
main = interact (unlines . zipWith format [1..] . map (solve.read) . tail.words)
format i j = "Case #" ++ show i ++ ": " ++ j
solve 0 = "INSOMNIA"
solve n = show $ go (accumArray undefined False ('0','9') []) n where
  go a i | and a' = i | otherwise = go a' (i+n) where
    a' = a // zip (show i) (repeat True)

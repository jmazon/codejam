import Data.Bits
main = interact (unlines . zipWith c [1..] . solve . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ if j then "ON" else "OFF"
solve (n:k:ns) = test n k : solve ns
solve _ = []
test :: Int -> Int -> Bool
test n k = k .&. m == m where m = 2^n - 1
-- SUCCESS
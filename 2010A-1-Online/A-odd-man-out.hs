import Data.List (foldl1')
import Data.Bits (xor)

main = interact (unlines . zipWith c [1..] . map solve . parse . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ show j
parse (n:ns) = ns' : parse ns'' where (ns',ns'') = splitAt n ns
parse _ = []
solve = foldl1' xor

-- SUCCESS

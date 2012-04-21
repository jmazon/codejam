import Data.Bits
import Data.List
main = interact (unlines . zipWith c [1..] . map solve . parse . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ maybe "NO" show j
parse (n:ns) = ns' : parse ns'' where (ns',ns'') = splitAt n ns
parse _ = []
solve ns = if foldl1' xor ns == 0
           then Just (sum ns - minimum ns)
           else Nothing

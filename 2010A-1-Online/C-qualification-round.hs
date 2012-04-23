import Data.List (sort)
import Debug.Trace

main = interact (unlines . zipWith c [1..] . map solve . parse . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ show j
parse (p:c:ns) = (c,sort ss) : parse ns' where (ss,ns') = splitAt p ns
solve (c,ss) | traceShow ss False = undefined
             | c > length ss = 0
             | otherwise     = n + 
                               solve (c,dropWhile (<= 0) 
                                           ((map (subtract n) as) ++ bs))
    where (as,bs) = splitAt c ss
          n = head ss

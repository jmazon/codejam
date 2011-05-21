import Data.List
main = interact $ unlines . zipWith c [1..] . map solve .
                  parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ show j
parse [] = []
parse (p:k:l:ns) = (p,k,ls) : parse ns'
    where (ls,ns') = splitAt l ns
solve (p,k,ls) = sum $ zipWith (*)
                 (reverse $ sort ls)
                 (concatMap (replicate k) [1..])
-- SUCCESS

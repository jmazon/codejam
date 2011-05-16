import Data.List
main = interact $ unlines . zipWith c [1..] . map solve .
                  parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ show j
parse [] = []
parse (n:ns) = (as,bs) : parse ns''
    where (as,ns') = splitAt n ns
          (bs,ns'') = splitAt n ns'
solve (as,bs) = sum $ zipWith (*) (sort as) (reverse $ sort bs)
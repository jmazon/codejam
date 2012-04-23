import Data.List (sort,foldl1',genericSplitAt,nub)
solve es = mod (-e) $ foldl1' gcd $ map (subtract e) es'
    where (e:es') = nub $ sort es
parse (n:ns) = es : parse ns' where (es,ns') = genericSplitAt n ns
parse _ = []
main = interact (unlines . zipWith c [1..] . map solve . parse . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ show j
-- SUCCESS
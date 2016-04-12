import Data.List
main = interact $ unlines . zipWith format [1..] . map solve .
                  triplets . map read . tail . words
format i j = "Case #" ++ show i ++ ": " ++
             maybe "IMPOSSIBLE" (unwords . map (show . succ)) j
triplets (a:b:c:x) = (a,b,c) : triplets x
triplets [] = []
solve (k,c,s) | n > s = Nothing | otherwise = Just gs where
    n = (k + c - 1) `div` c
    gs = genericTake n $ map pos $ chunksOf c $ cycle [0..k-1]
    pos = foldl1 (\a b -> a * k + b)
chunksOf n xs = l : chunksOf n r where (l,r) = genericSplitAt n xs

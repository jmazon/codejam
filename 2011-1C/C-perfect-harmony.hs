main = interact $ unlines . zipWith c [1..] . map solve . parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ maybe "NO" show j
parse [] = []
parse (n:l:h:ns) = (l,h,fs) : parse ns'
    where (fs,ns') = splitAt n ns

solve (l,h,fs) = case filter ok [l..h] of
                   (h:_) -> Just h
                   _ -> Nothing
    where ok f = all (g f) fs
          g f1 f2 | f1 <= f2   = f2 `mod` f1 == 0
                  | otherwise  = g f2 f1


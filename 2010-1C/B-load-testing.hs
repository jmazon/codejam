main = interact $ unlines . zipWith c [1..] . map solve . parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ show j
parse (l:p:c:ns) = (l,p,c) : parse ns
parse _ = []
solve (l,p,c) = max 0 $ ceiling (logBase 2 (log (p/l) / log c))

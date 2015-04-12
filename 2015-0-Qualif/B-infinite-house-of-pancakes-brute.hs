main = interact $ unlines . zipWith format [1..] . map solve .
                  parse . map read . tail . words
format i j = "Case #" ++ show i ++ ": " ++ show j
solve xs = minimum [ c n | n <- [1..1000] ] where
  c n = n + sum (map (pred . (`div` n) . (+ (n-1))) xs)
parse (n:xs) = ns : parse ys where (ns,ys) = splitAt n xs
parse _ = []

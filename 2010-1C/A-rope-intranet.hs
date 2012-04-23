main = interact $ unlines . zipWith c [1..] . map solve . parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ show j
parse [] = []
parse (n:ns) = pairs ws : parse ns'
    where (ws,ns') = splitAt (2*n) ns
          pairs (a:b:c) = (a,b) : pairs c
          pairs _ = []
solve [] = 0
solve ((a,b):ws) = solve ws +
                   sum [ if (a-a')*(b-b')<0 then 1 else 0 | (a',b') <- ws ]

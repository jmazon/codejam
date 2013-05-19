main = interact $ unlines . zipWith format [1..] . map (uncurry solve) .
                  pairs . map read . tail . words
format i j = "Case #" ++ show i ++ ": " ++ concat j
pairs (x:y:zs) = (x,y) : pairs zs
pairs _ = []
solve x y = (if x >= 0 then replicate x "WE" else replicate (-x) "EW") ++
            (if y >= 0 then replicate y "SN" else replicate (-y) "NS")

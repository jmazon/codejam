main = interact $ unlines . zipWith format [1..] . map solve . tail . lines
format i j = "Case #" ++ show i ++ ": " ++ j
solve = foldl f "" where f w l = max (l : w) (w ++ [l])

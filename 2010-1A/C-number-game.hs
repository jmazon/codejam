main = interact (unlines . zipWith c [1..] . map solve . parse . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ show j
parse (a:b:c:d:xs) = (a,b,c,d) : parse xs
parse _ = []
solve (a1,a2,b1,b2) = sum [ max b1 (min (b2+1) l) - b1 + b2 - min b2 (max (b1-1) h) | a <- [a1..a2], let l = ceiling (fromIntegral a / phi), let h = floor (fromIntegral a * phi)]
phi = (1 + sqrt 5) / 2

-- SUCCESS (compilation preferable for the large input set)

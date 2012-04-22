import Data.List

main = interact $ unlines . zipWith c [1..] . map solve . parse . map read . tail . words

c i j = "Case #" ++ show i ++ ": " ++ show (fromIntegral j / 2)

parse [] = []
parse (c:d:ns) = (d,p ps) : parse ns'
    where (ps,ns') = splitAt (2*c) ns
          p (a:b:cs) = (a,b) : p cs
          p _ = []

solve (d,ps) = maximum . map (uncurry y) . pairs . concat . snd $ mapAccumL x 0 ps
    where x i (p,v) = (i+v,[(i,p),(i+v-1,p)])
          y (i,pi) (j,pj) = d*(j-i) - (pj-pi)

pairs (x:xs) = map ((,) x) xs ++ pairs xs
pairs _ = []

-- N^2 but SUCCESS

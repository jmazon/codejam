import Data.Array
main = interact $ unlines . zipWith format [1..] . map solve .
                  parse . map read . tail . words
format i j = "Case #" ++ show i ++ ": " ++ show j
parse (e:r:n:xs) = let (ns,ns') = splitAt n xs
                   in (e,r,listArray (0,n-1) ns) : parse ns'
parse _ = []
solve (eMax,r,vs) = go 0 (snd (bounds vs) + 1) eMax r
    where go a b s e | a >= b    = 0
                     | otherwise = go a c s e' + n*v + go (c+1) b s' e
              where (v,c) = maximum [ (vs!i,i) | i <- [a..b-1]]
                    e' = min eMax (s + r * (c-a))
                    s' = max r (e - r * (b-c-1))
                    n = max 0 $ min e' $ e' + r - s'

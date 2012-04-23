import Data.List (sort)

readTests [] = []
readTests (n:ns) = sort (f ps) : readTests ns'
    where (ps,ns') = splitAt (2*n) ns
          f (x:y:xs) = (2*y,2*x) : f xs
          f _ = []

test tps d = test' d 0 0 (2*10^9) tps
test' d t l r ((t',p):tps) | l' <= r'  = test' d t' l' r' tps
                           | otherwise = False
    where l' = max (l-t'+t) (p-d)
          r' = min (r+t'-t) (p+d)
test' _ _ _ _ _ = True

bsearch f a b | b-a <= 1  = b
              | f c       = bsearch f a c
              | otherwise = bsearch f c b
    where c = (a+b) `div` 2

solve t = fromIntegral (bsearch (test t) (-1) (2*10^9)) / 2

main = interact (unlines . zipWith c [1..] . map solve . readTests . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ show j

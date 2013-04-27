import Data.List (find)
paint r n = n * (2*n + 2*r - 1)
solve r t = bsearch 1 b
    where Just b = find (>t) $ map (paint r) $ iterate (*2) 1
          bsearch a b | b == a+1      = a
                      | paint r c > t = bsearch a c
                      | otherwise     = bsearch c b
              where c = (a + b + 1) `div` 2
main = interact $ 
       unlines . zipWith format [1..] . map (uncurry solve) . 
       parse . map read . tail . words
format i j = "Case #" ++ show i ++ ": " ++ show j
parse (x:y:zs) = (x,y) : parse zs
parse [] = []

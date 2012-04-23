import Data.Array
import Data.List (sort,findIndex)

main = interact (unlines . zipWith c [1..] . map solve . parse . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ maybe "IMPOSSIBLE" (unwords . map show) j
parse [] = []
parse (n:t:e:ns) = (t,ts) : parse ns'
    where (es,ns') = splitAt (2 * e) ns
          ts = assocs $ accumArray (flip (:)) [] (1,n) $ pairwise es
          pairwise (x:y:zs) = (x,y) : pairwise zs
          pairwise _ = []
solve (t,ts) = sequence $ map (uncurry f) ts
    where f n es | n == t    = Just 0
                 | otherwise = findIndex (>= length es) . 
                               scanl (+) 0 $
                               reverse (sort es)

-- SUCCESS

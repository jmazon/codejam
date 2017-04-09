import Data.Array
import Data.Ord  (Down(Down),comparing)
import Data.List (find,maximumBy)
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
parse = pairs . map read . tail . words where
  pairs (a:b:cs) = (a,b) : pairs cs
  pairs [] = []
format x (y,z) = "Case #" ++ show x ++ ": " ++ show y ++ " " ++ show z
solve :: Int -> Int -> (Int,Int)
solve n = go stalls0 where
  stalls0 = accumArray (||) False (0,n+1) [(0,True),(n+1,True)]
  go st k | k == 1 = (h,l)
          | otherwise = go (st // [(best,True)]) (k-1) where
    d i = (min l r,max l r,Down i) where
      Just l = find ((st!) . (i - 1 -)) [0..]
      Just r = find ((st!) . (i + 1 +)) [0..]
    best = maximumBy (comparing d) $ filter (not . (st!)) $ [1..n]
    (l,h,_) = d best

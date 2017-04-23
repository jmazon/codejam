import Data.List (sort)
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
format x y = "Case #" ++ show x ++ ": " ++ show y
parse = go . tail . words where
  go [] = []
  go (d:n:r) = (read d,sort (pairs r')) : go r'' where
    (r',r'') = splitAt (2*read n) r
  pairs (a:b:cs) = (read a,read b) : pairs cs
  pairs [] = []
solve d = (d /) . maximum . map timeToDest where
  timeToDest (k,s) = (d-k) / s

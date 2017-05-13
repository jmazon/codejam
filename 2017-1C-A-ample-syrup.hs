import Data.Ord (Down(Down))
import Data.List (sortOn,tails)
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
format x y = "Case #" ++ show x ++ ": " ++ show y
parse = go . map read . tail . words where
  go [] = []
  go (n:k:ns) = (k,pairs ps) : go ns' where
    (ps,ns') = splitAt (2*n) ns
  pairs [] = []
  pairs (a:b:cs ) = (fromIntegral a,fromIntegral b) : pairs cs
solve k ps = maximum (go <$> tails sps) where
  sps = sortOn (Down . fst) ps
  go [] = 0
  go (p@(r,h):ps) = pi * r * r + sum (map side (p : take (k-1) (sortOn (Down . side) ps)))
  side (r,h) = 2 * pi * r * h

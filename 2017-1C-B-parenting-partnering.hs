import Data.Ord
import Data.List
import Data.Monoid
import Control.Arrow

main = interact $ unlines . zipWith format [1..] . map solve . parse
format x y = "Case #" ++ show x ++ ": " ++ show y
parse = go . map read . tail . words where
  go [] = []
  go (j:c:ns) = sort (zip as ps) : go ns' where
    (_as,ns') = splitAt ((j+c)*2) ns
    as = pairs (_as)
    ps = replicate j False ++ replicate c True
  pairs [] = []
  pairs (a:b:cs) = (a,b) : pairs cs
solve as = baseTransitions + go (baseDuration + committedTime) increases decreases where
  baseTransitions = sum (map holeCost hs)
  committedTime = sum (map p1Time as)
  baseDuration = sum (map holeDuration hs)
  p1Time ((t1,t2),False) = t2 - t1
  p1Time _ = 0
  hs = zipWith hole as (tail as ++ [head as])
  (increases,decreases) = (sort' *** sort') $ foldr partHole ([],[]) hs
  sort' = sortBy (comparing snd <> comparing (Down . fst))
  hole ((_,t1),p1) ((t2,_),p2)
    | p1 /= p2 = (1,0,Right d,0)
    | p1 == False = (0,d,Left d,2)
    | p1 == True = (0,0,Right d,2)
    where d = (t2-t1) `mod` 1440
  partHole (_,_,Left v,c) (is,ds) = (is,(v,c):ds)
  partHole (_,_,Right v,c) (is,ds) = ((v,c):is,ds)
  go d is ds | d == 720 = 0
             | d  < 720 = let ((v,c):is') = is; d' =  min (720-d) v
                          in c + go (d+d') is' ds
             | d  > 720 = let ((v,c):ds') = ds; d' = min (d-720) v
                          in c + go (d-d') is ds'
holeCost (c,_,_,_) = c
holeDuration (_,d,_,_) = d

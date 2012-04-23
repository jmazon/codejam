import Data.Array
import Data.List
import Control.Arrow

t1 = (5,[[(1,1)],[(1,0),(2,0)],[(5,0)]])
t2 = (1,[[(1,0)],[(1,1)]])

solve (n,cs) = go (listArray (1,n) (repeat 0)) cs
    where go a [] = Just (elems a)
          go a (c:cs') | or [ a!f==m | (f,m)<-c ] = go a cs'
                       | all ((==0) . snd) c      = Nothing
                       | otherwise                = go a' cs
              where a' = a // [x]
                    Just x = find ((==1) . snd) c

main = interact $ unlines . zipWith c [1..] . map solve .
                  parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ maybe "IMPOSSIBLE" present j
present j = intercalate " " (map show j)
parse [] = []
parse (n:m:ns) = (n,cs) : parse ns'
    where (cs,ns') = p m ns
          p 0 ns = ([],ns)
          p m (t:ts) = first (pairs ns :) $ p (m-1) ns'
              where (ns,ns') = splitAt (2*t) ts
          pairs (a:b:c) = (a,b) : pairs c
          pairs _ = []

-- SUCCESS

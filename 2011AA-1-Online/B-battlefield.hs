import Data.Array
import Data.Tree
import Data.Graph
import Data.Monoid
import Data.Foldable (fold)
import Control.Arrow
import Debug.Trace

tr x = traceShow x x

readTests [] = []
readTests (n:r:ns) = buildG (0,n-1) (f ps) : readTests ns'
    where (ps,ns') = splitAt (2 * r) ns
          f (x:y:xs) = (x,y) : f xs
          f _ = []

test t = merge $
         map (getSum . fold . fmap (m . odd . (d!))) $
         filter (not . null . subForest) $
         components t
    where d = accum (+) (indegree t) (assocs $ outdegree t)
          m False = Sum 0
          m True = Sum 1
          merge (0:0:xs) = 1 + merge (2:xs)
          merge (a:0:xs) = 1 + merge (a:xs)
          merge (0:b:xs) = 1 + merge (b:xs)
          merge (a:b:xs) = 1 + merge (a+b-2:xs)
          merge [n] = (n + 1) `div` 2

main = interact (unlines . zipWith c [1..] . map test . readTests . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ show j

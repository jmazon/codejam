import Data.Maybe (fromJust)
import Data.List (sort,elemIndex,elemIndices)
import Control.Monad (guard)

main = interact (unlines . uncurry (zipWith3 c [1..]) . unzip . map (uncurry solve) . readTests . map read . tail . words)

c i j k = "Case #" ++ show i ++ ": " ++ show j ++ " " ++ show k

readTests [] = []
readTests (c:i:ns) = (c,is) : readTests ns'
    where (is,ns') = splitAt i ns

solve c is = head $ do
  let is' = sort is
  p1 <- is'
  let is2 = elemIndices (c - p1) is
  guard $ not (null is2)
  let i1 = fromJust $ elemIndex p1 is
      is2' = filter (> i1) is2
  guard $ not (null is2')
  let i2 = head is2'
  return (1 + i1, 1 + i2)

-- SUCCESS

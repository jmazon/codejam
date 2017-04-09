import qualified Data.IntMap.Strict as M
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
parse = pairs . map read . tail . words where
  pairs (a:b:cs) = (a,b) : pairs cs
  pairs [] = []
format x (y,z) = "Case #" ++ show x ++ ": " ++ show y ++ " " ++ show z
solve n = go (M.singleton n 1) where
  go st k | k <= count = (high,low)
          | otherwise = go (M.insertWith (+) low count $
                            M.insertWith (+) high count st')
                           (k - count)
    where Just ((best,count),st') = M.maxViewWithKey st
          high = best `div` 2
          low = (best - 1) `div` 2

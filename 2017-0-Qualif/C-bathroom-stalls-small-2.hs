import Data.Ord (Down(Down))
import qualified Data.Set as S

main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
parse = pairs . map read . tail . words
  where pairs (a:b:cs) = (a,b) : pairs cs
        pairs [] = []
format x (y,z) = "Case #" ++ show x ++ ": " ++ show y ++ " " ++ show z
solve n = go (S.singleton (n,Down 0))
  where go st k | k == 1 = (high,low)
                | otherwise = i' `seq` go ( S.insert (low,Down i) $
                                            S.insert (high,Down i') st' )
                                          (k-1)
          where Just ((best,Down i),st') = S.maxView st
                high = best `div` 2
                low = (best - 1) `div` 2
                i' = i + low + 1

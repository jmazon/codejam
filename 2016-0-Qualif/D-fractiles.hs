import Data.List.Split (chunksOf)
import Data.List       (nub)
import Control.Monad   (guard)
main = interact $ unlines . zipWith format [1..] . map solve .
                  chunksOf 3 . map read . tail . words
format i j = "Case #" ++ show i ++ ": "
             ++ maybe "IMPOSSIBLE" (unwords . map show) j
solve [k,c,s] = guard (c * s >= k) >>
                return (nub (take s cross))
  where cross = map encode . chunksOf c $ [0..k-1] ++ repeat 0
        encode = succ . foldl (\a b -> k*a + b) 0

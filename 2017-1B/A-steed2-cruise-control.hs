import Data.List.Split (chunksOf)
import Control.Arrow   ((<<<),(&&&))
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
format x y = "Case #" ++ show x ++ ": " ++ show y
parse = readTest . tail . words
  where readTest [] = []
        readTest (d:n:r) = (read d,readHorses r') : readTest r''
          where (r',r'') = splitAt (2*read n) r
        readHorses = map ((!!0) &&& (!!1)) <<< chunksOf 2 <<< map read
solve d = (d /) . maximum . map timeToDest where timeToDest (k,s) = (d-k) / s

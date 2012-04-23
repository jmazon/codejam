import Data.Maybe
import Data.List
main = interact $ unlines . zipWith c [1..] . map solve . tail . lines
c i j = "Case #" ++ show i ++ ": " ++ show j
solve s = foldl1' g $ map (fromJust . flip lookup t) s
    where t = zip (nub s) (1:0:[2..])
          b = max 2 $ length t
          g x y = b*x+y

-- SUCCESS

import Data.List (group)
main = interact $ unlines . zipWith format [1..] . map solve . tail . lines
format i j = "Case #" ++ show i ++ ": " ++ show j
solve = length . dropWhile (== '+') . map head . group . reverse

import Data.List (group,sort)
main = interact $ unlines . zipWith format [1..] . map solve . parse
parse = problems . map read . tail . words where
  problems (n:ns) = xs : problems xs' where (xs,xs') = splitAt (n * (2*n-1)) ns
  problems [] = []
format i j = "Case #" ++ show i ++ ": " ++ unwords (map show j)
solve = map head . filter (odd . length) . group . sort

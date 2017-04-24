import Data.List (group)
main = interact $ unlines . zipWith format [1..] .
                  map solve . tail . lines
format x y = "Case #" ++ show x ++ ": " ++ y
solve = dropWhile (== '0') . go . group where
  go [ns] = ns
  go (as@(a:as'):bs) | let bs'@(b:_) = go bs, a <= b = as ++ bs'
                     | otherwise = pred a : map (const '9') (as' ++ concat bs)

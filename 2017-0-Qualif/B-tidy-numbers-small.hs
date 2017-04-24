import Data.List (sort)
main = interact $ unlines . zipWith format [1..] .
                  map solve . tail . lines
format x y = "Case #" ++ show x ++ ": " ++ y
solve n = show $ last $ filter tidy [1..read n] where
  tidy n = let r = show n in r == sort r

import Data.List (findIndices)
main = interact $ unlines . zipWith format [1..] .
                  map (uncurry solve . parse) . tail . lines
format i j = "Case #" ++ show i ++ ": " ++ show j
parse l = (a,read b) where [a,b] = words l
solve w n = go cs (n-1) 0
    where cs = findIndices (>= n) (clusters w)
          go (m:cs) i a = go cs m $! a + (m - i) * (length w - m + 1)
          go   _    _ a = a
clusters w = scanl go 0 w where go a c | isConsonnant c = a + 1
                                       | otherwise      = 0
isConsonnant = (`notElem` "aeiou")

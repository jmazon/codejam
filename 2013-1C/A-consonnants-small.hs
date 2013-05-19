import Data.List (inits,tails)
main = interact $ unlines . zipWith format [1..] .
                  map (uncurry solve . parse) . tail . lines
format i j = "Case #" ++ show i ++ ": " ++ show j
parse l = (a,read b) where [a,b] = words l
solve w n = length $ filter (hardToPronounce n 0) $ substrings w
hardToPronounce n a s @ ~(h:t) | a >= n         = True
                               | null s         = False
                               | isConsonnant h = hardToPronounce n (a+1) t
                               | otherwise      = hardToPronounce n   0   t
isConsonnant = (`notElem` "aeiou")
substrings = concatMap (tail . inits) . tails
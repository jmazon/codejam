main = interact $ unlines . zipWith format [1..] .
                  map (foldr solve ("","")) . tail . words
format i (a,b) = unwords [ "Case #" ++ show i ++ ":", a, b ]
solve '4' (a,b) = ('2':a,'2':b)
solve  n  (a,b) = ( n :a,'0':b)

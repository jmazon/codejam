import Data.Ord
import Data.List
main = interact $ unlines . zipWith format [1..] .
                  map (uncurry fine) . pairs . tail . words
format i (c,j) = "Case #" ++ show i ++ ": " ++ c ++ " " ++ j
pairs (a:b:c) = (a,b) : pairs c
pairs [] = []
fine = solve EQ [] []
solve _ ac aj [] [] = (reverse ac,reverse aj)
solve EQ ac aj ('?':cs) ('?':js) = minimumBy (comparing draw)
                                   [ solve EQ ('0':ac) ('0':aj) cs js
                                   , solve LT ('0':ac) ('1':aj) cs js
                                   , solve GT ('1':ac) ('0':aj) cs js ]
solve EQ ac aj ('?':cs) (j:js) = minimumBy (comparing draw) $
                                 solve EQ (j:ac) (j:aj) cs js :
                                 (if j > '0' then [ solve LT (pred j:ac) (j:aj) cs js ] else []) ++
                                 (if j < '9' then [ solve GT (succ j:ac) (j:aj) cs js ] else [])
solve EQ ac aj (c:cs) ('?':js) = minimumBy (comparing draw) $
                                 solve EQ (c:ac) (c:aj) cs js :
                                 (if c > '0' then [ solve GT (c:ac) (pred c:aj) cs js ] else []) ++
                                 (if c < '9' then [ solve LT (c:ac) (succ c:aj) cs js ] else [])
solve EQ ac aj (c:cs) (j:js)
  | c == j = solve EQ (c:ac) (j:aj) cs js
  | otherwise = solve (compare c j) (c:ac) (j:aj) cs js
solve LT ac aj (c:cs) (j:js) = solve LT (c':ac) (j':aj) cs js
  where c' | c == '?' = '9'
           | otherwise = c
        j' | j == '?' = '0'
           | otherwise = j
solve GT ac aj cs js = uncurry (flip (,)) $ solve LT aj ac js cs
draw (cs,js) = (abs (c - j),c,j) where c = read cs; j = read js

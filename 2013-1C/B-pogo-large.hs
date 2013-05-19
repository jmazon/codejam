import Data.Maybe
seek 0 0 a 0                 = Just a
seek _ _ _ n | n <= 0        = Nothing
seek x y a n | abs x > abs y = if x > 0 then seek (x-n) y ('E':a) (n-1)
                                        else seek (x+n) y ('W':a) (n-1)
             | otherwise     = if y > 0 then seek x (y-n) ('N':a) (n-1)
                                        else seek x (y+n) ('S':a) (n-1)
solve x y = head $ catMaybes $ map (seek x y []) [0..]
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) .
                  pairs . map read . tail . words
format i j = "Case #" ++ show i ++ ": " ++ j
pairs (x:y:zs) = (x,y) : pairs zs
pairs _ = []

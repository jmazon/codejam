data Worse = RICHARD | GABRIEL deriving (Enum,Show)
betterToWorse = toEnum . fromEnum :: Enum better => better -> Worse
main = interact $ unlines . zipWith format [1..] . 
                  map (betterToWorse . uncurry3 solve) .
                  triplets . map read . tail . words
format i j = "Case #" ++ show i ++ ": " ++ show j
uncurry3 f (x,y,z) = f x y z
triplets (x:y:z:ns) = (x,y,z) : triplets ns
triplets _ = []
solve x r c = better x r c && worse x (min r c)
better x r c = ((r * c) `mod` x) == 0 && max r c >= x
worse 3 = (> 1)
worse 4 = (> 2)
worse 5 = (> 3)
worse 6 = (> 3)
worse x = const (x < 4)

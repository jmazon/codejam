import Data.Char (digitToInt)
main = interact $ unlines . zipWith solve [1..] . map (map digitToInt . snd) .
                  filter fst . zip (cycle [False,True]) . tail . words
solve i = (("Case #" ++ show i ++ ": ") ++) . show . fst . foldl go (0,(0,0))
    where go (f,(t,i)) n | t >= i    = (f,    (t+n,i+1))
                         | otherwise = (f+i-t,(n+i,i+1))

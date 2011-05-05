import Data.Array.Unboxed
import Data.List
main = interact (unlines . zipWith c [1..] . map solve . parse . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ show j
parse (d:i:m:n:ns) = (d,i,m,n,ns') : parse ns'' where (ns',ns'') = splitAt n ns
parse _ = []
solve (d,i,m,n,ns) = minimum . elems $
                     foldl' (solve' d i m)
                     (listArray (0,255) (repeat 0))
                     ns
solve' d i m a n = listArray (0,255) $ map s [0..255] :: UArray Int Int
    where s f = minimum $ (d + a!f) : 
                          if m > 0
                          then [ mc + ic + a!p
                               | p <- [0..255],
                                 let mc = abs (f-n),
                                 let ic = i * max 0 ((abs (f-p) - 1) `div` m) ]
                          else [abs (f-n) + a!f]

-- SUCCESS (needs compilation for large input)

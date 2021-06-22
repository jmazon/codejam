{-# LANGUAGE FlexibleContexts #-}

import Data.Array.Unboxed
import Data.List

main :: IO ()
main = interact (unlines . zipWith c [1..] . map solve . parse . map read . tail . words)

c :: Int -> Int -> String
c i j = "Case #" ++ show i ++ ": " ++ show j

parse :: [Int] -> [(Int,Int,Int,Int,[Int])]
parse (d:i:m:n:ns) = (d,i,m,n,ns') : parse ns'' where (ns',ns'') = splitAt n ns
parse _ = []

solve :: (Int,Int,Int,Int,[Int]) -> Int
solve (d,i,m,_n,ns) = minimum . elems $
                      foldl' (solve' d i m)
                      (listArray (0,255) (repeat 0))
                      ns

solve' :: Int -> Int -> Int -> UArray Int Int -> Int -> UArray Int Int
solve' d i m a n = listArray (0,255) $ map s [0..255] :: UArray Int Int
    where s f = minimum $ (d + a!f) : 
                          if m > 0
                          then [ mc + ic + a!p
                               | p <- [0..255],
                                 let mc = abs (f-n),
                                 let ic = i * max 0 ((abs (f-p) - 1) `div` m) ]
                          else [abs (f-n) + a!f]

-- SUCCESS (needs compilation for large input)

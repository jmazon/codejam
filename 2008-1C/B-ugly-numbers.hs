import Data.Char
import Data.List
import Data.Array
import Debug.Trace

solve s = sum [ f 1 1!r | r <- [0..209], ugly r ]
    where n = length s
          a = listArray (1,n) $ map ((subtract $ ord '0') . ord) s
          c = array ((-1,1),(1,n))
              [ ((m,i),f m i) | m <- [-1..1], i <- [1..n+1] ]
          f _ i | i > n = listArray (0,209) (1:repeat 0)
          f m i = accumArray (+) 0 (0,209)
                  [ ((r + m * x i j) `mod` 210, c!(m',j+1)!r)
                    | j <- [i..n], m' <- [-1,1], r <- [0..209] ]
          x i j = foldl1' h [ a!k | k <- [1..j] ]
          h a b = 10*a + b

ugly n = n `mod` 2 == 0 ||
         n `mod` 3 == 0 ||
         n `mod` 5 == 0 ||
         n `mod` 7 == 0 

-- not there yet
main = putStrLn "not there yet"

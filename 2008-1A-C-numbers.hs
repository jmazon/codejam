import Control.Monad
import Text.Printf

(a,b,c,d)×(e,f,g,h) = ( (a*e+b*g) `mod` 1000
                      , (a*f+b*h) `mod` 1000
                      , (c*e+d*g) `mod` 1000
                      , (c*f+d*h) `mod` 1000 )

m·0 = (1,0,0,1)
m·1 = m
m·n | even n    = m'×m'
    | otherwise = m×m'×m'
    where m' = m·n'
          n' = n `div` 2

solve n = (2*a-1) `mod` 1000 where (a,_,_,_) = (3,5,1,3)·n

main = zipWithM_ c [1..] . map solve . map read . tail . words =<< getContents
c :: Int -> Int -> IO ()
c i j = printf "Case #%d: %03d\n" i j

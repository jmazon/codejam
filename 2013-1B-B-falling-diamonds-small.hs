import Data.Array
main = interact $ unlines . zipWith format [1..] . map solve .
                  parse . map read . tail . words
format i j = "Case #" ++ show i ++ ": " ++ show j
parse (n:x:y:zs) = (n,x,y) : parse zs
parse _ = []
solve (n,x,y) = go n 0
    where ring = (abs x + abs y) `div` 2
          go n i | n <= 0      = 0
                 | i > ring    = 1
                 | i < ring    = go (n - 4*i - 1) (i+1)
                 | y == 2*i    = if n >= 4*i + 1 then 1 else 0
                 | n > y + 2*i = 1
                 | n > y       = fromIntegral (sum [ choose n j | j <- [0..n-y-1] ]) / fromIntegral (2^n)
                 | otherwise   = 0
choose = curry (a!)
    where a = array ((0,0),(1000,1000)) [ ((i,j),f i j) | i <- [0..1000], j <- [0..1000] ]
          f n p | p < 0 || p > n = 0
          f 0 _                  = 1
          f _ 0                  = 1
          f n p                  = a!(n-1,p-1) + a!(n-1,p)

import Data.List (sort,nub)
main = interact $ unlines . zipWith format [1..] . map solve . parse . lines
format i (Just j) = "Case #" ++ show i ++ ": " ++ j
parse = go . tail where go (_:l:ls) = map read (words l) : go ls
                        go []       = []
solve products = mapM (`lookup` lt) primes where
  (left,right) = skipEqual [head products] (tail products)
  skipEqual as bs@(b:bs') | head as == b = skipEqual (b:as) bs'
                          | otherwise    = (as,bs)
  middle = gcd (head left) (head right) :: Integer
  primes = reverse (scanl (flip div) middle left) ++
           tail    (scanl (flip div) middle right)
  lt = zip (sort (nub primes)) ['A'..'Z']

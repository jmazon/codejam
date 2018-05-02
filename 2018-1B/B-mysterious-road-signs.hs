import Data.List (mapAccumL)
main = interact $ unlines . zipWith format [1..] . map solve . parse
format x (y,z) = "Case #" ++ show x ++ ": " ++ show y ++ " " ++ show z
parse = go . map read . tail . words where
  go (s:xs) = triplets ss : go xs' where (ss,xs') = splitAt (3*s) xs
  go [] = []
  triplets (a:b:c:xs) = (a,b,c) : triplets xs
  triplets [] = []
solve = foldl countMax (0,undefined) . snd .
        mapAccumL f (st0,st0) . zip [0..] . map toMN where
  toMN (d,a,b) = (d+a,d-b)
  invalid = 10^8
  st0 = (invalid,invalid,0,0)
  f (ms@(mm,mn,mi,mpi),ns@(nm,nn,ni,npi)) (i,(m,n)) = ((ms',ns'),max mb nb) where
    (ms',mb) | m == mm = (ms,i+1-mi)
             | m == nm = ((nm,nn,ni,i),i+1-ni)
             | otherwise = ((m,nn,npi,i),i+1-npi)
    (ns',nb) | n == nn = (ns,i+1-ni)
             | n == mn = ((mm,mn,mi,i),i+1-mi)
             | otherwise = ((mm,n,mpi,i),i+1-mpi)
  countMax (m,c) v | v > m = (v,1)
                   | v == m = (m,c+1)
                   | otherwise = (m,c)

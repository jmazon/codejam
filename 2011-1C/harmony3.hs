import Data.List

main = interact $ unlines . zipWith c [1..] . map solve . parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ maybe "NO" show j
parse [] = []
parse (n:l:h:ns) = (l,h,sort fs) : parse ns'
    where (fs,ns') = splitAt (fromIntegral n) ns

solve :: (Integer,Integer,[Integer]) -> Maybe Integer
solve (l,h,fs) = case concatMap s field' ++ right of
                   (a:_) -> Just a
                   _ -> Nothing
    where lcms = scanl lcm 1 fs
          gcds = reverse (scanl1 gcd $ reverse fs)
          field = zip (zip (1:fs) fs) (zip lcms gcds)
          field' = dropWhile ((<l) . snd . fst) $
                   takeWhile ((<=h) . fst . fst) field
          s ((a,b),(m,d)) = filter ((==0) . (mod d)) $
                            takeWhile (<=min h b) [m1,m1+m..]
              where m1 = lm m (max l a)
          lcm' = last lcms
          lcm'' = lm lcm' l
          right = if lcm'' <= h then [lcm''] else []

lm m t = m * ((t+m-1) `div` m)

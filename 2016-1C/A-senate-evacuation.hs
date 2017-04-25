import qualified Data.Set as S

main = interact $ unlines . zipWith format [1..] . map solve . parse
parse = map (map read . words . snd) . filter fst .
        zip (cycle [False,True]) . tail . lines
format x y = "Case #" ++ show x ++ ": " ++ unwords y
solve ns = go (sum ns) (S.fromList $ zip ns ['A'..]) where
  go s m | s <= 2    = [concatMap (uncurry replicate) (S.elems m)]
         | s == 3    = [p1]    : go (s-1) m'
         | otherwise = [p1,p2] : go (s-2) m''
    where (p1,m' ) = extract m
          (p2,m'') = extract m'
  extract m | n == 1    = (p,m')
            | otherwise = (p,S.insert (n-1,p) m')
    where Just ((n,p),m') = S.maxView m

import Control.Monad
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    putStrLn $ "Case #" ++ show i ++ ":"
    dna <- fmap words getLine
    let d = read (dna !! 0)
        n = read (dna !! 1)
    ps <- fmap (pairs . map read . words . unlines) (replicateM n getLine)
    as <- fmap (map read . words) getLine
    let (ts,xs) = unzip (trunc d ps)
    mapM_ (print . solve n ts xs) as
solve n ts xs a = last ts' + maximum (0:ds)
    where ts' = map (\x -> sqrt (2 * x / a)) xs
          ds = zipWith (-) ts ts'
trunc d ((t1,x1):(t2,x2):ps)
    | x2 > d    = [(t1,x1),(t1+(t2-t1)*(d-x1)/(x2-x1),d)]
    | x2 == d   = [(t1,x1),(t2,x2)]
    | otherwise = (t1,x1) : trunc d ((t2,x2):ps)
trunc d [(t,_)] = [(t,d)]
pairs (x:y:zs) = (x,y) : pairs zs
pairs _ = []

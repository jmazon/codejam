import Control.Arrow ((&&&),second)
import Control.Monad (forM_,replicateM)
import Data.List (minimumBy,partition,findIndices,nub)
import Data.Function (on)

partitionBy p xs = map (map fst . flip filter l . (.snd) . (==)) ks
    where l = map (id &&& p) xs
          ks = nub (map snd l)

maximumsOn p = go (-1) []
    where go _ a [] = a
          go m a (x:xs) | m' > m    = go m' [x]  xs
                        | m' == m   = go m (x:a) xs
                        | otherwise = go m   a   xs
              where m' = p x

solve ws ls = fst $ minimumBy (compare `on` snd) $
              map fst $ maximumsOn snd $
              concatMap (solve' ls) (partitionBy (length.fst) (zip ws [1..]))

solve' _ [] = []
solve' _ [x] = [(x,0)]
solve' (l:ls) xs
    | any (elem l.fst) xs = ( concatMap (solve' ls) $
                              partitionBy (findIndices (==l).fst) r )
                                ++ 
                            ( map (second succ) $ solve' ls w )
    | otherwise           = solve' ls xs
    where (r,w) = partition (elem l.fst) xs

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [n,m] <- fmap (map read . words) getLine
    ws <- replicateM n getLine
    rs <- replicateM m $ fmap (solve ws) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ unwords rs

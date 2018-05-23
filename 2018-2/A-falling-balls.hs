import Data.Array
import Data.List
import Control.Monad
main = do
  t <- readLn
  forM [1..t] $ \tt -> do
    c <- readLn
    bs <- map read . words <$> getLine
    let res = do
          guard (sum bs == c)
          guard (head bs > 0)
          guard (last bs > 0)
          let ds = snd $ mapAccumL dest 1 bs
              rs = concat $ zipWith3 (construct 2) [1..] bs ds
              h = maximum $ 1 : map (fst . fst) rs
              g = accumArray (flip const) '.' ((1,1),(h,c)) rs
          return $ show h : [ [ g!(i,j) | j <- [1..c] ] | i <- [h,h-1..1] ]
    putStr $ "Case #" ++ show tt ++ ": " ++ maybe "IMPOSSIBLE\n" unlines res
dest cur c = (cur+c,(cur,cur+c))
construct h i c (a,b) = constructL ++ constructR where
  constructL = zip (zip [2..] [i-1,i-2..a]) (repeat '\\')
  constructR = zip (zip [2..] [i+1,i+2..b-1]) (repeat '/')

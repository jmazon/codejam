import Data.Array    (listArray,(!))
import Data.Array.ST (runSTArray,newListArray,readArray,writeArray)
import Control.Monad (forM_,replicateM)
import Control.Arrow (first,(&&&))
import qualified Data.Set as S

readPair l = (e,s) where [e,s] = read <$> words l

main = do
  t <- readLn
  forM_ [1..t] $ \tn -> do
    [n,q] <- map read . words <$> getLine
    hs    <- listArray (1,n) . map readPair <$> replicateM n getLine

    _ds   <- map read . concatMap words <$> replicateM n getLine
    let ds = runSTArray $ do
          dist <- newListArray ((1,1),(n,n)) $
                  (\d -> if d < 0 then 10^12 else d) <$> _ds
          sequence_ $ fw dist <$> [1..n] <*> [1..n] <*> [1..n]
          return dist
        fw dist k i j = do
          d <- (+) <$> readArray dist (i,k) <*> readArray dist (k,j)
          writeArray dist (i,j) . min d =<< readArray dist(i,j)

    let dijkstra src dst = go (S.fromList [1..n]) (S.singleton (0,src)) where
          go open q | node == dst             = dist
                    | node `S.notMember` open = go open q'
                    | otherwise               = go open' q''
            where Just ((dist,node),q') = S.minView q
                  (e,s) = hs ! node
                  open' = node `S.delete` open
                  ns = S.map (first (\d -> dist + d/s)) $
                       S.filter     (\(d,_) -> d <= e)  $
                       S.map       ((\i -> ds!(node,i)) &&& id) open'
                  q'' = S.union q' ns

    rs <- replicateM q (uncurry dijkstra . readPair <$> getLine)
    putStrLn $ "Case #" ++ show tn ++ ": " ++ unwords (show <$> rs)

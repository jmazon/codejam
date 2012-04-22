import Control.Monad
import Data.List
main = zipWithM_ format [1..] . map solve . unfoldr parse .
       map read . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ show j
solve (s,p,ts) = length pn + min s (length ps)
    where (pn,fn) = partition ((>= p) . bestNormal) ts
          ps = filter ((>= p) . bestSurprising) $
               filter (<= 28) $ filter (>= 2) fn
bestNormal n = (n + 2) `div` 3
bestSurprising n = (n + 4) `div` 3
parse (n:s:p:ns) = Just ((s,p,ts),ns') where (ts,ns') = splitAt n ns
parse _ = Nothing
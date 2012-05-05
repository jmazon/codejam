import Control.Monad
import Data.List
import Control.Arrow
import Data.Ord
main = zipWithM_ format [1..] . map solve .
       unfoldr parse . map read . tail . words =<< getContents
format i j = do
  putStrLn $ "Case #" ++ show i ++ ":"
  case j of
    Nothing -> putStrLn "Impossible"
    Just (k,l) -> do
      putStrLn $ unwords (map show k)
      putStrLn $ unwords (map show l)
parse (n:ns) = Just (ps,ns') where (ps,ns') = splitAt n ns
parse _ = Nothing
solve ns = firstPair . sortBy (comparing snd) . map (id &&& sum) $ filterM (const [False,True]) ns
firstPair (x:y:zs) | snd x == snd y = Just (fst x,fst y)
                   | otherwise      = firstPair (y:zs)
firstPair _ = Nothing

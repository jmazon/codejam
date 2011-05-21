import Data.Maybe
import Data.List (sort,nub,delete,intercalate)
import Data.Sequence (ViewL(..),(><))
import qualified Data.Sequence as S
import Control.Monad

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    k <- readLn
    ds <- fmap (map read . tail . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ intercalate " " (map show (solve ds k))

-- start k = S.iterateN k succ (1::Int)
start k = S.fromList [1..k]
extract k s = (e,s1 >< s2)
    where (s1,s') = S.splitAt k s
          e :< s2 = S.viewl s'

solve ds = catMaybes . flip map ds . flip lookup .
           snd . head . filter (null . fst) . scanl f (nub ds,[]) .
           go 0 0 . start
    where go p i s | S.null s  = []
                   | otherwise = (e,i+1) : go p' (i+1) s'
              where (e,s') = extract p' s
                    p' = (p + i) `mod` S.length s
          f (ds,a) (e,i) | e `elem` ds = (delete e ds,(e,i):a)
                         | otherwise   = (ds,a)

-- SUCCESS

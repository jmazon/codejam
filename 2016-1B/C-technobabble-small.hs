import Data.Ord
import Data.Bits
import Data.List
import qualified Data.Map.Strict as M
import Control.Arrow
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse . tail . lines
format i j = "Case #" ++ show i ++ ": " ++ show j
parse (_n:ls) = (n,map pair ts) : parse ls' where n = read _n; (ts,ls') = splitAt n ls
parse [] = []
pair l = (a,b) where [a,b] = words l
solve n _ts = (n -) $ popCount $ head $ filter covers $ sortBy (comparing popCount) [0 .. 2^n-1]
  where
    ((_,nl,_,nr),ts) = mapAccumL doubleIntern (M.empty,0,M.empty,0) _ts
    doubleIntern (ml,il,mr,ir) (l,r) = ((ml',il',mr',ir'),(li,ri)) where
      (ml',il',li) = intern ml il l
      (mr',ir',ri) = intern mr ir r
    intern m i w | Just j <- M.lookup w m = (m,i,j)
                 | otherwise = (M.insert w i m,i+1,i)
    covers :: Int -> Bool
    covers bs = cv == (2^nl-1,2^nr-1)
      where
        cv = foldl' (\(a,b) (c,d) -> (setBit a c,setBit b d)) (0,0) $ bts :: (Int,Int)
        bts = map fst $ filter snd $ zip ts $ bits :: [(Int,Int)]
        bits = map (testBit bs) [0..n-1] :: [Bool]

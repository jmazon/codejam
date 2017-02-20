import Data.List (groupBy,partition,sort)
import Data.Function (on)
import qualified Data.IntMap as M

data Segment = Chain { chEnd :: !Int, chSize :: !Int }
             | Loop  {                lpSize :: !Int }
               deriving (Eq,Ord)
isLoop (Loop    _) = True
isLoop (Chain _ _) = False

main = interact $ unlines . zipWith format [1..] . map solve . parse
format i j = "Case #" ++ show i ++ ": " ++ show j
parse = problems . map read . tail . words where
  problems [] = []
  problems (n:ns) = map pred xs : problems xs' where (xs,xs') = splitAt n ns
solve ns = maximum $ bestChains : map lpSize loops where
  bestChains = sum $ map (maximum . map chSize) $
               groupBy ((==) `on` chEnd) chains
  (loops,chains) = partition isLoop $ sort $ map (trav M.empty (-1)) $
                   zipWith (curry fst) [0..] ns
  trav all prev cur
    | Just occ <- M.lookup cur all = Loop (M.size all - occ)
    | ns !! cur == prev            = Chain cur (M.size all)
    | otherwise = trav (M.insert cur (M.size all) all) cur (ns !! cur)

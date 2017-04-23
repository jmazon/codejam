import Data.Ord        (Down(Down))
import Data.Array      (Ix,Array,listArray,(!),accum,(//))
import Data.List       (find,sortOn)
import Data.List.Split (chunksOf)
import Control.Monad   (guard)
import Control.Arrow   ((>>>),(&&&))

main = interact $ unlines . zipWith format [1..] . map solve . parse
parse = map toHerd . chunksOf 7 . map read . tail . words
  where toHerd (_:hs) = listArray (R,V) hs
format x y = "Case #" ++ show x ++ ": " ++ maybe "IMPOSSIBLE" (concatMap show) y

data Mane = R | O | Y | G | B | V deriving (Show,Enum,Eq,Ord,Ix)
primaries = [R,Y,B]
primariesFrom R = [Y,B]
primariesFrom Y = [B,R]
primariesFrom B = [R,Y]
compositeFrom R = G
compositeFrom Y = V
compositeFrom B = O

solve h0 = find ((> 0) . (h0!)) primaries >>= \c -> go c [c] h0' where
  h0' = accum (-) h0 $ map (\p -> (p,h0!compositeFrom p)) primaries
  go i ss@(u:ss') h
    | all (== 0) h = guard (u == i) *> pure ss'
    | otherwise    = do (u',op) <- find ((> 0) . (h!) . fst) (neighs u)
                        go i (op ss) (h // [(u',h!u'-1)])
    where neighs = (:) <$> assessComposite <*> choosePrimary
          choosePrimary = primariesFrom >>>
                          sortOn (Down . (h!) &&& (== i)) >>>
                          map (id &&& (:))
          assessComposite p = (c, ((p:) . (c:))) where c = compositeFrom p

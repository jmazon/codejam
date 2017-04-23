import Data.Ord        (comparing)
import Data.Monoid     ((<>))
import Data.Array.Unboxed
import Data.List       (maximumBy)
import Data.List.Split (chunksOf)
import Control.Monad   (guard)

main = interact $ unlines . zipWith format [1..] . map solve . parse
parse = map toHerd . chunksOf 7 . map read . tail . words
  where toHerd (_:hs) = listArray (R,V) hs
format x y = "Case #" ++ show x ++ ": " ++ maybe "IMPOSSIBLE" (concatMap show) y

data Mane = R | O | Y | G | B | V deriving (Show,Enum,Eq,Ord,Ix)
solve :: UArray Mane Int -> Maybe [Mane]
solve h | h!R > 0 = go R [R] h
        | h!Y > 0 = go Y [Y] h
        | h!B > 0 = go B [B] h
        | otherwise = Nothing
  where
    go i ss@(u:ss') h | sum (elems h) == 0   = guard (u == i) >> return ss'
                      | Just u' <- optimal u = go i (u':ss) (commit u' h)
                      | Just u' <- natural u = go i (u':ss) (commit u' h)
                      | otherwise = Nothing
      where
        optimal u = do
          u' <- preference u
          guard (h!u' > 0)
          return u'
        preference R = Just G
        preference Y = Just V
        preference B = Just O
        preference _ = Nothing

        natural u = do
          let u' = maximumBy (comparing (h!) <> comparing (/= i)) (neighs u)
          guard (h!u' > 0)
          return u'
        neighs R = [Y,B]
        neighs Y = [B,R]
        neighs B = [R,Y]
        neighs G = [R]
        neighs V = [Y]
        neighs O = [B]

        commit u h = h // [(u,h!u-1)]

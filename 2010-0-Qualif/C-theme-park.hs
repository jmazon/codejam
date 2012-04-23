import Data.Array
import Data.List
import qualified Data.IntMap as M
main = interact (unlines . zipWith c [1..] . map solve . parse . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ show j
parse (r:k:n:ns) = (r,k,gs) : parse ns' where (gs,ns') = splitAt n ns
parse _ = []
solve (r,k,gs) = solve' r k g (listArray (0,g-1) gs) 0 (Just M.empty) 0
    where g = length gs
solve' 0 _ _ _ _ _ a = a
solve' r k g gs i m a = solve' r' k g gs i' m' $! a'
    where (di,n) = last $ filter ((<=k) . snd) $
                   zip [1..] $ scanl1 (+) . elems $
                   ixmap (bounds gs) adj $ gs
          adj k = (k + i) `mod` g
          (r',a',i',m') = case m >>= M.lookup i of
                         Just (or,oa) -> (rem, a + (a-oa)*q, i, Nothing)
                             where (q,rem) = r `divMod` (or-r)
                         Nothing -> (r-1,a+n,(i+di) `mod` g,
                                     M.insert i (r,a) `fmap` m)
-- SUCCESS
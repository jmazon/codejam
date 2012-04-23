import Control.Arrow
import Data.List (unfoldr,partition,sortBy)

main = interact (unlines . zipWith c [1..] . top . words)
c i j = "Case #" ++ show i ++ ": " ++ show j

top (n:ws) = unfoldr test (read n,ws)
test (0,_) = Nothing
test (i,(s:ws)) = Just (r,(i-1,ws'))
    where (sls,ws') = splitAt (read s) ws
          (bs,rs) = f *** f $
                    partition ((=='R') . last) sls
          f = sortBy (flip compare) . map (pred . read . init)
          r = sum $ zipWith (+) bs rs

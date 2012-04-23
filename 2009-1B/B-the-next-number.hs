import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
next_perm p = dropWhile (=='0') $ elems $ runSTUArray a'
    where a = listArray (0,n) ('0' : p) :: UArray Int Char
          n = length p
          k = maximum [ k | k <- [0..n-1], a!k < a!(k+1) ]
          l = maximum [ l | l <- [k+1..n], a!k < a!l ]
          a' = do
            a' <- thaw a :: ST s (STUArray s Int Char)
            writeArray a' k (a!l)
            writeArray a' l (a!k)
            mapIndices (0,n) f a'
          f i | i <= k    = i
              | otherwise = n+k+1-i
main = interact $ unlines . zipWith c [1..] . map next_perm . tail . lines
c i j = "Case #" ++ show i ++ ": " ++ j
-- SUCCESS

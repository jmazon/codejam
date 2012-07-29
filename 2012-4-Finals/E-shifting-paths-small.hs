import Data.Bits
import Data.List (unfoldr)
import Data.Array
import qualified Data.IntSet as S
import Control.Monad (zipWithM_)
main = zipWithM_ format [1..] . map solve . parse . map read . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ maybe "Infinity" show j
solve :: Array Int (Int,Int) -> Maybe Int
solve f = go 0 S.empty 0
    where go x s l | loop      = Nothing
                   | done      = Just l
                   | otherwise = go x' s' $! l+1
            where loop = S.member x s
                  done = not $ inRange (bounds f) c
                  c = x `shiftR` 40
                  c' = (if testBit x c then snd else fst) (f!c)
                  x' = complementBit x c .&. 0x0000000fffffffff .|.
                       c' `shiftL` 40
                  s' = S.insert x s
parse (n:ns) = listArray (0,n-2) (unfoldr toPairs ps) : parse ns'
    where (ps,ns') = splitAt (2*n-2) ns
          toPairs (a:b:xs) = Just ((a-1,b-1),xs)
          toPairs _ = Nothing
parse _ = []

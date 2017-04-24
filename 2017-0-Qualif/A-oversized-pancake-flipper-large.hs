import Data.Bits     (xor,shiftL,setBit,testBit)
import Data.List     (foldl',findIndices)
import Control.Monad (guard)

main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
parse = pairs . tail . words where
  pairs (a:b:cs) = (a,read b) : pairs cs
  pairs [] = []
format x y = "Case #" ++ show x ++ ": " ++ maybe "IMPOSSIBLE" show y
solve ps k = go 0 p0 0 where
  n = length ps
  flipper = 2^k - 1 :: Integer
  p0 = foldl' setBit 0 (findIndices (== '-') ps)
  go i p a | i > n-k = guard (p == 0) >> return a
           | testBit p i = go (i+1) (p `xor` (flipper `shiftL` i)) $! a+1
           | otherwise = go (i+1) p a

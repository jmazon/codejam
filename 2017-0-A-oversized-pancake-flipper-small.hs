import Data.Bits     (xor)
import Data.Maybe    (listToMaybe)
import Control.Monad (filterM)
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
parse = pairs . tail . words where
  pairs (a:b:cs) = (a,read b) : pairs cs
  pairs [] = []
format x y = "Case #" ++ show x ++ ": " ++ maybe "IMPOSSIBLE" show y
solve ps k = length <$> listToMaybe solutions where
  n = length ps
  starts = filterM (const [False,True]) [0..n-k]
  apply starts = go 0 False ps starts starts where
    go _ _ [] _ _ = []

    go i f (p:ps) ss es =
        (if f' then flip p else p) : go (i+1) f' ps ss' es'
      where
        flip '-' = '+'
        flip '+' = '-'
        (isStart,ss') | null ss || head ss /= i = (False,ss)
                      | otherwise = (True,tail ss)
        (isEnd,es') | null es || head es /= i-k = (False,es)
                    | otherwise = (True,tail es)
        f' = f `xor` isStart `xor` isEnd
  verify = all (== '+')
  solutions = filter (verify . apply) starts

import Data.MemoTrie

solve ps = s where
  s = memo2 s'
  s' a b = case takeWhile (<= b) $ dropWhile (<a) ps of
             [] -> 0
             ps' -> minimum [ b - a + s a (p-1) + s (p+1) b | p <- ps' ]

splits ns = go ns []
    where go (n:ns) a = (a,n,ns) : go ns (a++[n])
          go _ _ = []

main = interact $ unlines . zipWith c [1..] . test . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ show j
test [] = []
test (p:q:ns) = solve qs 1 p : test ns'
    where (qs,ns') = splitAt q ns

-- SUCCESS

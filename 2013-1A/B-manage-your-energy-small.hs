-- brute force, but it's enough to pass small
main = interact $ unlines . zipWith format [1..] . map solve .
                  parse . map read . tail . words
format i j = "Case #" ++ show i ++ ": " ++ show j
parse (e:r:n:xs) = (e,r,ns) : parse ns' where (ns,ns') = splitAt n xs
parse _ = []
solve (eMax,r,vs) = maximum $ go eMax vs 0
    where go e   []   a = return a
          go e (v:vs) a = do
            s <- [0..e]
            go (min eMax (e - s + r)) vs $! a + s*v

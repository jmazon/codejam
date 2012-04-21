main = interact $ unlines . zipWith c [1..] . map solve . parse . tail . words
c i j = "Case #" ++ show i ++ ": " ++ show j
parse [] = []
parse (n:xs) = f d : parse d'
    where (d,d') = splitAt (2*read n) xs
          f (a:b:c) = (a=="O",read b) : f c
          f _ = []
solve = go 0 1 0 1 0
    where go pt pp ot op t ((True,p):x) = go t' p ot op t' x
              where t' = 1 + max t (pt + abs (p - pp))
          go ot op pt pp t ((False,p):x) = go ot op t' p t' x
              where t' = 1 + max t (pt + abs (p - pp))
          go _ _ _ _ t _ = t

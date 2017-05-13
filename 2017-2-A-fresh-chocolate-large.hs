main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
parse = go . map read . tail . words where
  go [] = []
  go (n:p:ns) = (p,gs) : go ns' where (gs,ns') = splitAt n ns
format x y = "Case #" ++ show x ++ ": " ++ show y
solve p gs = case p of 2 -> go2
                       3 -> go3
                       4 -> go4
  where residue n = length $ filter ((== n) . (`mod` p)) gs
        go2 = residue 0 + residue 1 `div` 2 + residue 1 `mod` 2
        go3 = residue 0 + pairs12 + triplets + anythingLeft
          where
            mod1 = residue 1
            mod2 = residue 2
            pairs12 = min mod1 mod2
            mod1' = mod1 - pairs12
            mod2' = mod2 - pairs12
            triplets = mod1' `div` 3 + mod2' `div` 3
            mod1'' = mod1' `mod` 3; mod2'' = mod2' `mod` 3
            anythingLeft = signum (mod1'' + mod2'')
        go4 = residue 0 + pairs13 + pairs22 + triplets112 + quatrs + anythingLeft
          where
            mod1 = residue 1
            mod2 = residue 2
            mod3 = residue 3

            pairs13 = min mod1 mod3
            pairs22 = mod2 `div` 2

            mod1' = mod1 - pairs13
            mod2' = mod2 `mod` 2
            mod3' = mod3 - pairs13

            triplets112 = min (mod1' `div` 2) mod2'

            mod1'' = mod1' - 2 * triplets112
            mod2'' = mod2' - triplets112
            mod3'' = mod3'

            quatrs = mod1'' `div` 4 + mod3'' `div` 4

            mod1''' = mod1'' `mod` 4
            mod2''' = mod2''
            mod3''' = mod3'' `mod` 4

            anythingLeft = signum (mod1''' + mod2''' + mod3''')

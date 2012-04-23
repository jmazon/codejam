main = mapM_ putStrLn . zipWith present [1..] . map (solve . parse . tail . words) . tail . lines =<< getContents
present i x = "Case #" ++ show i ++ ": " ++ show x
parse (r:p:xs) = (head r == 'B', read p) : parse xs
parse _ = []
solve p = max a b
    where ((_,a),(_,b)) = foldl f ((1,0),(1,0)) p
          f ((xo,to),(xb,tb)) (c,p)
              | c         = ((xo,to),(p,1 + max to (tb + abs (xb-p))))
              | otherwise = ((p,1 + max tb (to + abs (xo-p))),(xb,tb))

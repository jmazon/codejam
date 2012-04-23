solve [_,pd,0  ] = pd == 0
solve [_,pd,100] = pd == 100
solve [n,pd,_  ] = 100 `div` gcd 100 pd <= n
format a b = "Case #" ++ show a ++ ": " ++ if b then "Possible" else "Broken"
main = mapM_ putStrLn . zipWith format [1..] . map (solve . map read . words) . tail . lines =<< getContents
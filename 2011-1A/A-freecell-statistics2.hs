main = interact $ unlines . zipWith c [1..] . map solve .
                  parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ present j
present True = "Possible"
present False = "Broken"
parse [] = []
parse (n:pd:pg:ns) = (n,pd,pg) : parse ns
solve (_,0,0) = True
solve (_,_,0) = False
solve (_,100,100) = True
solve (_,_,100) = False
solve (n,pd,_) = 100 `div` gcd 100 pd <= n
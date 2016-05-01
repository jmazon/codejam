import Data.List
main = interact $ unlines . zipWith format [1..] . map solve . tail . lines
format i j = "Case #" ++ show i ++ ": " ++ j
solve nb = sort $ replicate z '0' ++ replicate ones '1' ++
                  replicate w '2' ++ replicate threes '3' ++
                  replicate u '4' ++ replicate fives '5' ++
                  replicate x '6' ++ replicate sevens '7' ++
                  replicate g '8' ++ replicate nines '9'
  where
    count l = length (elemIndices l nb)
    fives = f - u
    threes = h - g
    ones = o - z - w - u
    sevens = v - fives
    nines = (n - ones - sevens) `div` 2
    ls@[z,e,r,o,n,t,w,h,f,u,i,v,s,x,g] = map count "ZERONTWHFUIVSXG"
    

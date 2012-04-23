import Data.Array.Unboxed
main = interact $ unlines . zipWith c [1..] . map solve . parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ show j
parse [] = []
parse (n:m:ns) = (n,m,listArray ((1,1),(n,m)) (triplets cs)::Array (Int,Int) (Int,Int,Int)) : parse ns'
    where (cs,ns') = splitAt (3*n*m) ns
          triplets (a:b:c:d) = (a,b,c) : triplets d
          triplets [] = []
solve (n,m,cs) = iterate (bf cs) i !! (2*m*n) ! (1,m,False,True)
    where i :: UArray (Int,Int,Bool,Bool) Int
          i = (accumArray min (maxBound `div` 2)
                          ((1,1,False,False),(n,m,True,True))
                          [((n,1,True,False),0)])
bf cs s = accum min s $
          filter ((inRange (bounds s)) . fst) $
          concatMap f (assocs s)
    where f ((i,j,v,h),t) = [ ((i+if v then 1 else (-1),j,not v,h),t+2)
                            , ((i,j+if h then 1 else (-1),v,not h),t+2)
                            , ((i,j,not v,h),if r < si
                                             then t+1
                                             else ti+(1+q)*p+1)
                            , ((i,j,v,not h),if r >= si
                                             then t+1
                                             else ti+q*p+si+1) ]
              where (si,wi,ti) = cs!(i,j)
                    p = si + wi
                    (q,r) = (t-ti) `divMod` p

(t1:t2:ts) = parse . map read . tail . words $
             "2 1 1 3 2 10 1 2 1 5 3 1 5 2"

-- unsolved (I think)

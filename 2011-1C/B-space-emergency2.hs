import Control.Monad
import Debug.Trace

main = interact $ unlines . zipWith c [1..] . map solve . parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ show j

parse [] = []
parse (l:t:n:c:ns) = (l,t,n,take n (cycle cs)) : parse ns'
    where (cs,ns') = splitAt c ns

solve (l,t,n,cs) = minimum $ do
  ls <- replicateM l [1..n]
  when (l>1) $ guard (ls!!0 < ls!!1)
  return $ travel t cs ls 0

--travel t cs ls a | traceShow (t,cs,ls,a) False = undefined
travel _ [] _ a = a
travel t (c:cs) (1:ls) a = if t <= 0
                           then travel (t-c) cs (map pred ls) $! a+c
                           else if t < 2*c 
                           then travel (t`div`2 - c) cs (map pred ls) $! a+c+t`div`2
                           else travel (t-2*c) cs (map pred ls) $! a+2*c
travel t (c:cs) ls a = travel (t-2*c) cs (map pred ls) $! a+2*c



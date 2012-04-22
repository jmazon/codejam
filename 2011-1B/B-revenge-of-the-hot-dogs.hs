import Data.List

split d (p,v) = (p-d*h,p+d*h,d*(h-0.5))
    where h = fromIntegral v / 2

overlap (a1,b1,t1) (a2,b2,t2) = a2 < b1
merge cs = groupBy overlap cs
fuse [c] = c
fuse cs = (a1-h,b2+h,t + h)
    where ((a1,b1,_):cs') = cs
          (b2,o) = ov b1 cs' 0
          ov b ((a',b',t'):cs) ac = ov b' cs $! ac + b - a'
          ov b [] ac = (b,ac)
          t = maximum $ map thrd cs
          h = o/2

thrd (_,_,x) = x
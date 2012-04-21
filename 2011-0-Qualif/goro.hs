import Debug.Trace
p n 1 = 1/n
p n k = (n-1)^2 * p (n-1) (k-1) / fact n
fact = product . enumFromTo 1
c 0 = error "c 0"
c 1 = 0
c n = (n + sum [c i + c' (n-i) | i <- [1..n-1]]) / (n-1)
c' 0 = 0
c' n = sum [c i + c' (n-i) | i <- [1..n]] / n

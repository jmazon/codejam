main = putStrLn "Case #1:" >>
       interact (format . solve . map read . words)
format = unlines . map (unwords . map show)
solve [1,n,j] = map (gen . toDec 1) [1..j] where
  toDec b 0 = 0
  toDec b n = r + 10*toDec (10*b) q where (q,r) = n `divMod` 2
  n2 = n `div` 2
  duplicate b = b^n2 + 1
  gen i = (10^(n2 - 1) + 10*i + 1) * duplicate 10 : map duplicate [2..10]

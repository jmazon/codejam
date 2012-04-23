import Control.Monad

main = do
  n <- readLn
  forM_ [1..n] $ \i -> do
    s <- readLn
    ss <- replicateM s getLine
    q <- readLn
    qs <- replicateM q getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (batch s qs)

batch s = go (s-1) [] 0
    where go _ _ a [] = a
          go n l a (q:qs) | q `elem` l = go n l a qs
                          | n > 0      = go (n-1) (q:l) a qs
                          | otherwise  = a' `seq` go (s-1) [] a' (q:qs)
              where a' = a+1

-- SUCCESS

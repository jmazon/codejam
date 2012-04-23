import Control.Monad (forM_,replicateM,guard)
import Data.Array

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    putStrLn $ "Case #" ++ show i ++ ":"
    [r,c] <- fmap (map read . words) getLine
    a <- fmap (solve r c . listArray ((1,1),(r,c)) . concat)
              (replicateM r getLine)
    putStr $ maybe "Impossible\n" (unlines . rows r c) a

rows r c a = [ [ a!(i,j) | j <- [1..c] ] | i <- [1..r] ]

solve r c = go 1 1
    where go i j a | j > c         = go (i+1) 1 a
                   | i > r         = return a
                   | a!(i,j) == '#' = do
                       guard $ j < c
                       guard $ a!(i  ,j+1) == '#'
                       guard $ i < r
                       guard $ a!(i+1,j  ) == '#'
                       guard $ a!(i+1,j+1) == '#'
                       go i (j+2) $ a // [ ((i,  j), '/'), ((i  ,j+1),'\\')
                                         , ((i+1,j),'\\'), ((i+1,j+1), '/') ]
                   | otherwise = go i (j+1) a

import Data.Array
import Data.List
import Control.Monad
import Control.Arrow

trees n a b c d x0 y0 m = take n $ go x0 y0
    where go x y = (x,y) : go ((a*x+b) `mod` m) ((c*y+d) `mod` m)

solve ts = sum (map c3 $ elems g) + sum ( do
             i <- [0..2]
             j <- [0..2]
             i' <- [0..2]
             j' <- [0..2]
             guard ((i,j) < (i',j'))
             let i'' = (3-i-i') `mod` 3
                 j'' = (3-j-j') `mod` 3
             guard ((i',j') < (i'',j''))
             return (g!(i,j) * g!(i',j') * g!(i'',j'')) )
    where g = accumArray (+) 0 ((0,0),(2,2))
              [ ((x `mod` 3,y `mod` 3),1) | (x,y) <- ts ]
          c3 n = n * (n-1) * (n-2) `div` 6

main = do
  n <- readLn
  forM_ [1..n] $ \i -> do
    [n,a,b,c,d,x0,y0,m] <- fmap (map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve $ trees n a b c d x0 y0 m)
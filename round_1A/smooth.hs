import Control.Applicative
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    [d,i,m,n] <- map read . words <$> getLine
    ns <- map read . words <$> getLine
    print $ solve d i m ns

solve _ _ _ [a] = 0
solve d i m [a,b] | s <= m = 0
                  | otherwise = minimum [ d, i * (1 + (s-1) `div` m), s - m ]
  where s = abs (a-b)
solve d i m [a,b,c] | a <= b && b <= c = solveOrdered d i m a b c
                    | a <= b && b >= c = solvePeak d i m a b c
                    | a >= b && b >= c = solveOrdered d i m a b c
                    | a >= b && b <= c = solvePeak d i m a b c

solveOrdered d i m a b c | s1 <= m && s2 <= m = 0
                         | s1 <= m && s2 >  m = solveOrdered d i m c b a
                         | s1 >  m && s2 <= m = solve d i m [a,b]
                         | s1 >  m && s2 >  m =
    minimum [ d + solve d i m [b,c]
            , d + solve d i m [a,b]
            , i * (1 + (s1-1) `div` m) + solve d i m [b,c]
            , i * (1 + (s2-1) `div` m) + solve d i m [a,b]
            , 
  where s1 = abs (a-b)
        s2 = abs (c-b)
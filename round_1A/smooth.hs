import Data.Array.Unboxed
import Data.Array.ST
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \j -> do
    [d,i,m,n] <- map read . words <$> getLine
    ns <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show j ++ ": " ++ show (solve d i m n ns)

solve :: Int -> Int -> Int -> Int -> [Int] -> Int
solve d i m n ns = minimum [ c!(n,f) | f <- [0..255] ]
    where a = listArray (1,n) ns :: UArray Int Int
          c = runSTUArray $ do
                t <- newArray_ ((0,0),(n,255))
                forM_ [0..n] $ \n ->
                  forM_ [0..255] $ \f -> do
                    v <- s' t n f
                    writeArray t (n,f) v
                return t
          s t n f = readArray t (n,f)
          s' t 0 f             = return 0
          s' t n f | m > 0     = do
            dc <- s t (n-1) f
            pc <- mapM (s t (n-1)) [0..255]
            return $ minimum $ (dc + d) :
                               zipWith (+) [ abs (f - a!n) +
                                             i * max 0 ((abs (f - p) - 1) `div` m)
                                             | p <- [0..255] ]
                                           pc
                   | otherwise = (+ min d (abs (f - a!n))) <$> s t (n-1) f 

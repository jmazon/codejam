{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Array
import Data.List (sort)
import Control.Monad (forM_,liftM,replicateM)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,k] <- liftM (map read . words) getLine
    as <- liftM (reverse . sort . map parseActivity) $ replicateM n getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve k as)

solve :: Int -> [(Double,Int)] -> Double
solve k as = minimum results
    where m = sum (map snd as)
          prefixes = scanl (flip mXm) (ident 3) . map act . expand $ take' k as
          suffixes = scanr (flip mXm) (ident 3) . map act . expand $ drop' (m-k) as
          results = zipWith (\a b -> b `mXm` a `mXv` fromList [1,0,0] @> 2)
                    prefixes suffixes

expand :: [(b,Int)] -> [b]
expand = concatMap (uncurry (flip replicate))

take',drop' :: Int -> [(a,Int)] -> [(a,Int)]
take' n ((p,c):as) | c < n     = (p,c) : take' (n-c) as
                   | otherwise = [(p,n)]
drop' n ((p,c):as) | n > c     = drop' (n-c) as
                   | otherwise = (p,c-n) : as

act :: Double -> Matrix Double
act p = (3><3) [  p ,  0 , 0
               , 1-p, 1-p, 0
               ,  0 ,  p , 1 ]

parseActivity :: String -> (Double,Int)
parseActivity l = (r,read f)
    where r = read n / read d
          [n,d,f] = words (map unRatio l)

unRatio :: Char -> Char
unRatio '/' = ' '
unRatio  x  =  x

------------------------------------------------------------
-- Poor man's hmatrix

type Matrix = Array (Int,Int)
type Vector = Array Int

ident :: Num a => Int -> Matrix a
ident n = accumArray (flip const) 0 ((0,0),(n-1,n-1)) [ ((i,i),1) | i <- [0..n-1] ]

mXm :: Num a => Matrix a -> Matrix a -> Matrix a
mXm m n | b /= b' = error "Nonconformant matrices"
        | otherwise = array ((0,0),(a,c))
                      [ ((i,k),sum [ m!(i,j) * m!(j,k) | j <- [0..b] ])
                      | i <- [0..a], k <- [0..c] ]
  where ((0,0),(a,b)) = bounds m
        ((0,0),(b',c)) = bounds n

mXv :: Num a => Matrix a -> Vector a -> Vector a
mXv m v | b' /= b = error "Nonconformant matrix/vector pair"
        | otherwise = array (0,a)
                      [ (i,sum [ m!(i,j) * v!j | j <- [0..b] ])
                      | i <- [0..a] ]
  where ((0,0),(a,b)) = bounds m
        (0,b') = bounds v

fromList :: [a] -> Vector a
fromList xs = listArray (0,length xs-1) xs

(@>) :: Vector a -> Int -> a
(@>) = (!)

(><) :: Int -> Int -> [a] -> Matrix a
a >< b = listArray ((0,0),(a-1,b-1))

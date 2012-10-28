import Data.List (sort,foldl')
import Control.Monad (forM_,liftM,replicateM)
import Numeric.Container

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [n,k] <- liftM (map read . words) getLine
    as <- liftM (reverse . sort . map parseActivity) $ replicateM n getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve k as)

solve k as = minimum results :: Double
    where m = sum (map snd as)
          prefixes = scanl (flip mXm) (ident 3) . map act . expand $ take' k as
          suffixes = scanr (flip mXm) (ident 3) . map act . expand $ drop' (m-k) as
          results = zipWith (\a b -> b `mXm` a `mXv` fromList [1,0,0] @> 2)
                    prefixes suffixes

woken (_,_,w) = w
expand = concatMap (uncurry (flip replicate))
take' n ((p,c):as) | c < n     = (p,c) : take' (n-c) as
                   | otherwise = [(p,n)]
drop' n ((p,c):as) | n > c     = drop' (n-c) as
                   | otherwise = (p,c-n) : as

act p = (3><3) [  p ,  0 , 0
               , 1-p, 1-p, 0
               ,  0 ,  p , 1 ]

parseActivity l = (r,read f)
    where r = fromIntegral (read n) / fromIntegral (read d)
          [n,d,f] = words (map unRatio l)
unRatio '/' = ' '
unRatio  x  =  x

import Data.List (sort,foldl')
import Control.Monad (forM_,liftM,replicateM)

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [n,k] <- liftM (map read . words) getLine
    as <- liftM (reverse . sort . map parseActivity) $ replicateM n getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve k as)

solve k as = minimum [ eval (take' q as ++ drop' (m-k+q) as) | q <- [0..k] ]
    where m = sum (map snd as)

take' n ((p,c):as) | c < n     = (p,c) : take' (n-c) as
                   | otherwise = [(p,n)]
drop' n ((p,c):as) | n > c     = drop' (n-c) as
                   | otherwise = (p,c-n) : as
eval = woken . foldl' act (1,0,0) . expand
woken (_,_,w) = w
expand = concatMap (uncurry (flip replicate))

act (a,s,w) p = (a',s',w')
    where a' =   p  *a
          s' = (1-p)*a + (1-p)*s
          w' =             p  *s + w
  
parseActivity l = (r,read f)
    where r = fromIntegral (read n) / fromIntegral (read d)
          [n,d,f] = words (map unRatio l)
unRatio '/' = ' '
unRatio  x  =  x

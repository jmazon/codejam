{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List
import Control.Monad
import Control.Arrow
import Control.Applicative

dup a = (a,a)
b = 10^11

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    putStrLn $ "Case #" ++ show i ++ ":"
    n <- readLn
    ns <- map (round . (*b)) . sort <$> replicateM n readLn
    mapM_ print $ map ((/b) . fromIntegral) $ solve 1 $ dup <$> ns

solve _ [] = []
--solve 30 ns = fst <$> ns
solve i ns = (fst <$> as) ++ solve i (second (`mod` b) <$> bs)
    where (as,bs) = partition (m . snd) $ second (*3) <$> ns
          m x = x >= 1*b && x <= 2*b
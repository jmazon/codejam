{-# LANGUAGE FlexibleInstances #-}
import Data.Array
import Data.List
import Data.Function
import Control.Monad
import System.IO

(a,b)+:(c,d) = (a+c,b+d)

main = do
  hSetBuffering stdout LineBuffering
  t <- readLn
  forM_ [1..t] $ \i -> do
    [h,w] <- fmap (map read . words) getLine
    m <- fmap (listArray ((1,1),(h,w)) . map read . concatMap words)
              (replicateM h getLine) :: IO (Array (Int,Int) Int)
    let s = listArray (bounds m) [d c | c <- indices m]
        d c = if n == c then n else s!n
            where n = minimumBy (compare `on` (m!)) $
                      filter (inRange (bounds m)) $
                      map (c+:) [ (0,0), (-1,0), (0,-1), (0,1), (1,0) ]
        b = listArray (bounds m) $ snd $ mapAccumL f ('a',[]) (elems s)
        f (n,l) c = case lookup c l of
                      Just s -> ((n,l),s)
                      Nothing -> ((succ n,(c,n):l),n)
    putStrLn $ "Case #" ++ show i ++ ":"
    forM_ [1..h] $ \i -> putStrLn $ intersperse ' ' [b!(i,j) | j <- [1..w]]

-- SUCCESS

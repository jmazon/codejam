{-# LANGUAGE TupleSections #-}

import Data.Char
import Data.List
import Data.Function
import Control.Monad

main = do
  n <- readLn
  forM_ [1..n] $ \i -> do
    t <- readLn
    [na,nb] <- fmap (map read . words) getLine
    as <- replicateM na (fmap (map readTime . words) getLine)
    bs <- replicateM nb (fmap (map readTime . words) getLine)
    let da = map ((,1) . (!!0)) as
        db = map ((,1) . (!!0)) bs
        aa = map ((,-1) . (+t) . (!!1)) bs
        ab = map ((,-1) . (+t) . (!!1)) as
        fa = maximum $ scanl (+) 0 $ map (sum . map snd) $ groupBy ((==) `on` fst) $ sort (da++aa)
        fb = maximum $ scanl (+) 0 $ map (sum . map snd) $ groupBy ((==) `on` fst) $ sort (db++ab)
    putStrLn $ "Case #" ++ show i ++ ": " ++ show fa ++ " " ++ show fb

readTime (a:b:_:c:d:_) = ((r a*10 + r b)*6 + r c)*10 + r d
    where r n = ord n - ord '0'

-- SUCCESS
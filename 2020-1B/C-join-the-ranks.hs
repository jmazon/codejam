import Control.Monad
import Data.Ord
import Data.Maybe
import Data.List

import Debug.Trace

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [r,s] <- map read . words <$> getLine
    let os = solve r s
    putStr $ unlines ( ("Case #" ++ show i ++ ": " ++ show (length os)) : map format os )
format (i,j) = show i ++ " " ++ show j

solve :: Int -> Int -> [(Int,Int)]
solve r s = go [ i | j <- [1..s], i <- [1..r] ]
  where go deck = case seekSplit deck of
          Nothing -> []
          Just (wa,wb) -> let (a,a') = splitAt wa deck
                              (b,s) = splitAt wb a'
                          in (wa,wb) : go (b ++ a ++ s)

        seekSplit :: [Int] -> Maybe (Int,Int)
        seekSplit d@(ta:_) = fmap snd $ mfilter ((> 0) . fst) $ listToMaybe $ sortOn (Down . fst)
          [ (eval bb ta + eval ba ts - eval ba tb - eval bb ts,(wa,wab-wa))
          | (_,ba):b@((wa,tb):_) <- tails (zip [0..] d)
          , (_,bb):(wab,ts):_ <- tails b ]

eval :: Int -> Int -> Int
eval r1 r2 | r1 == r2 = 2
           | r1+1 == r2 = 1
           | otherwise = 0

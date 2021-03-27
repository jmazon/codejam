{-# OPTIONS_GHC -Wall #-}

import Control.Monad
import Data.List
import Data.Array

default (Int)

main :: IO ()
main = do
  t <- readLn
  getLine -- p
  forM_ [1..t] $ \i -> do
    fs <- replicateM 100 (map (== '1') <$> getLine)
    let difficulties = remap $ map (length . filter not) (transpose fs)
        skills = remap $ map (length . filter not) fs
        scoreDiffs = zipWith scoreDiff (elems skills) fs
        scoreDiff s rs = sum $ zipWith (qScoreDiff s) (elems difficulties) rs
        qScoreDiff s q r = (fromIntegral (fromEnum r) - 1 / (1 + exp (q - s))) ^2
        (_,cheater) = maximum $ zip scoreDiffs [1..]
    putStrLn $ "Case #" ++ show i ++ ": " ++ show cheater

remap :: [Int] -> Array Int Double
remap xs = array (1,n) $
           zipWith (\y (_,i) -> (i,y)) [ -3 + 6 * fromIntegral j / fromIntegral (n-1) | j <- [0..n-1] ] $
           sort $
           zipWith (,) xs [1..]
  where n = length xs

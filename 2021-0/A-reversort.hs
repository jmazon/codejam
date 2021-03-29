{-# OPTIONS_GHC -Wall #-}

module Main where
import Control.Monad
import Control.Monad.Writer.Strict

default (Int)

minSplit :: [Int] -> Maybe (Int,[Int],[Int])
minSplit [] = Nothing
minSplit (x:t)
  | Just (m,h',t') <- minSplit t, m < x = Just (m,x:h',t')
  | otherwise                           = Just (x,[],t)

reversort :: [Int] -> Writer (Sum Int) [Int]
reversort xs = case minSplit xs of
  Just (m,h,t) -> do
    tell (Sum (1 + length h))
    (m :) <$> reversort (reverse h ++ t)
  Nothing -> pure xs

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    getLine -- n
    ls <- map read . words <$> getLine :: IO [Int]
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (getSum (execWriter (reversort ls)) - 1)

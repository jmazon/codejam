{-# OPTIONS_GHC -Wall #-}

import Control.Monad

default (Int)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [n,c] <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++
      maybe "IMPOSSIBLE" (unwords . map show) (revershuffle n c)

revershuffle :: Int -> Int -> Maybe [Int]
revershuffle n c0 = go 0 n [] (c0-n+1) where
  go _ _ _ c | c < 0 = Nothing
  go _ 0 xs c = guard (c == 0) *> pure xs
  go l x xs c =
    let step = min l c
        (h,t) = splitAt step xs
    in go (l+1) (x-1) (reverse (x : h) ++ t) (c-step)

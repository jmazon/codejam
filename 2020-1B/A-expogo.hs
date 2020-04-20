{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Data.Bits
import Data.Maybe
import Control.Monad
import Control.Monad.Writer

main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
parse = pairs . map read . tail . words where
  pairs (a:b:xs) = (a,b :: Int) : pairs xs
  pairs [] = []
format i j = "Case #" ++ show i ++ ": " ++ fromMaybe "IMPOSSIBLE" j
solve = (execWriterT .) . go where
  go 0 0 = lift (pure ())
  go 0 1 = tell "N"
  go 1 0 = tell "E"
  go 0 (-1) = tell "S"
  go (-1) 0 = tell "W"
  go x y = case (x `testBit` 0,y `testBit` 0) of
    (True,False) -> handle x y "WE" >>= \x' -> go (x' `quot` 2) (y `quot` 2)
    (False,True) -> handle y x "SN" >>= \y' -> go (x `quot` 2) (y' `quot` 2)
    _ -> mzero
  handle a b dirs
    | x = tell [ dirs !! fromEnum (a > 0) ] *> pure (a - signum a)
    | otherwise = tell [ dirs !! fromEnum (a < 0) ] *> pure (a + signum a)
    where x = (abs a `xor` abs b) `testBit` 1

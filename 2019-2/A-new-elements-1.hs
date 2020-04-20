module Main (main) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Ratio
import qualified Data.Set as S

main :: IO ()
main = interact $ unlines . zipWith format [1..] . map solve . parse

format :: Int -> Int -> String
format i j = "Case #" ++ show i ++ ": " ++ show j

parse :: String -> [[Molecule]]
parse = go . map read . tail . words where
  go (n:xs) = pairs ps : go xs' where (ps,xs') = splitAt (2*n) xs 
  go [] = []
  pairs (a:b:xs) = M a b : pairs xs
  pairs [] = []

data Molecule = M !Int !Int deriving Show

pairToRatio :: Molecule -> Molecule -> Maybe Rational
pairToRatio (M c1 j1) (M c2 j2) = guard (dj /= 0 && q > 0) *> Just q
  where dc = c2 - c1
        dj = j2 - j1
        q = - fromIntegral dc % fromIntegral dj

solve :: [Molecule] -> Int
solve = succ . S.size . S.fromList . mapMaybe (uncurry pairToRatio) . pairs
  where pairs xs = [ (x,y) | (x:ys) <- tails xs, y <- ys ]

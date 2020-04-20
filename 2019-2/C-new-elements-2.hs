module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Ord
import Data.Ratio
import Data.List
import Data.Monoid

main :: IO ()
main = interact $ unlines . zipWith format [1..] . map solve . parse

format :: Int -> Maybe (Integer,Integer) -> String
format i j = "Case #" ++ show i ++ ": " ++ maybe "IMPOSSIBLE" unpair j
  where unpair (a,b) = show a ++ " " ++ show b

parse :: String -> [[Molecule]]
parse = go . map read . tail . words where
  go (n:xs) = pairs ps : go xs' where (ps,xs') = splitAt (2*n) xs 
  go [] = []
  pairs (a:b:xs) = M a b : pairs xs
  pairs [] = []
  pairs _ = error "pairs: odd number of elements"

data Range = R !Rational !Rational deriving Show
inf :: Num n => n
inf = 10^10
emptyRange, fullRange :: Range
emptyRange = R inf 0
fullRange = R 0 inf
boolRange :: Bool -> Range
boolRange False = emptyRange
boolRange True = fullRange
inter :: Range -> Range -> Range
inter (R a b) (R c d) = R (max a c) (min b d)
validRange :: Range -> Bool
validRange (R a b) = a < b

data Molecule = M !Int !Int deriving Show

pairToRange :: Molecule -> Molecule -> Range
pairToRange (M c1 j1) (M c2 j2) = case compare dc 0 of
                                    EQ -> boolRange (dj > 0)
                                    GT -> R q inf
                                    LT -> R 0 q
  where dc = c2 - c1
        dj = j2 - j1
        q = - fromIntegral dj % fromIntegral dc

solve :: [Molecule] -> Maybe (Integer,Integer)
solve = (search =<<) . mfilter validRange . pure .
        foldl1' inter . (zipWith pairToRange =<< tail)

search :: Range -> Maybe (Integer,Integer)
search (R a b) = case qs' of
                   (q:_) -> pure (numerator q,denominator q)
                   [] -> Nothing
  where
    cas = (++ repeat inf) <$> fracToCont a
    cbs = (++ repeat inf) <$> fracToCont b
    cqs = foldr best undefined <$> liftA2 zip cas cbs

    qs = map contFrac $ concatMap inits cqs
    m = (a + b) / 2
    hw = (b - a) / 2
    qs' = sortBy (comparing numerator <> comparing denominator) $
          filter ((> 0) . denominator) $
          filter ((> 0) . numerator) $
          filter ((< hw) . abs . (m -)) qs

best :: (Integer,Integer) -> [Integer] -> [Integer]
best (a,b) r | a == b = a : r
             | otherwise = [min a b + 1]

contFrac :: [Integer] -> Rational
contFrac [x] = fromIntegral x
contFrac (x:y:_) | y >= inf = fromIntegral x
contFrac (x:xs) = fromIntegral x + recip (contFrac xs)
contFrac [] = 0

fracToCont :: Rational -> [[Integer]]
fracToCont q = case properFraction q of
  (0,0) -> [[0]]
  (n,0) -> [[n],[n-1,1]]
  (n,r) -> (n :) <$> fracToCont (recip r)

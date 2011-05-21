{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}

import Data.Array.Unboxed
import Data.List (foldl',unfoldr)
import Data.Monoid
import Data.FingerTree

data Elem = Elem Int (Sum Int) deriving Show
instance Measured (Sum Int) Elem
type F = FingerTree (Sum Int) Elem

solve :: [Int] -> F
solve = foldl' f empty
    where f q x = l >< ( (Elem x (v `mappend` (Sum 1) `mappend` (measure l))) <| r' )
              where (l,r) = split ((>= x) . fst) q
                    (v,r') = case viewl r of
                               EmptyL -> (mempty,r)
                               ((Elem k v):<r') -> if k==x then (v,r') else (mempty,r)

rnd (n,m,x,y,z,as) = unfoldr f (0,listArray (0,m-1) as :: UArray Int Int)
    where f (i,a) | i < n     = Just ( a!(i`mod`m)
                                     , (i+1,a //
                                       [ ( i`mod`m
                                         , (x*(a!(i`mod`m))+y*(i+1))`mod`z)]))
                  | otherwise = Nothing

main = interact $ unlines . zipWith c [1..] . map (solve . rnd) . parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ show j
parse [] = []
parse (n:m:x:y:z:ns) = (n,m,x,y,z,as) : parse ns'
    where (as,ns') = splitAt m ns

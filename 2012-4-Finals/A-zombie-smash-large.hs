{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Ord (comparing)
import Data.List (unfoldr,inits,tails,insertBy)
import qualified Data.HashMap.Strict as M
import Control.Monad.State
import Control.Monad.List

main :: IO ()
main = zipWithM_ format [1..] . map solve . parse . map read . tail . words =<< getContents

format :: Int -> Int -> IO ()
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ show j

dist :: Int -> Int -> Int -> Int -> Int
dist x1 y1 x2 y2 = max (abs (x2 - x1)) (abs (y2 - y1))

solve :: [(Int,Int,Int)] -> Int
solve = flip evalState M.empty . smashable True 0 0 0
    where smashable _ _ _ _ [] = return 0
          smashable c x y t zs = liftM (maximum . (0:)) . runListT $ do
            ((x',y',ta),zs') <- ListT $ return (pick zs)
            let td = ta + 1000
                tr = t + max (if c then 0 else 750) (100 * dist x y x' y')
                tk = max ta tr
            guard $ tr <= td
            lift $ do
              let k = (x',y',ta)
              tkvs <- liftM (maybe [] id . M.lookup k) get
              case segLookup tk tkvs of
                Just v -> return v
                Nothing -> do
                  v <- liftM (+1) $ smashable False x' y' tk zs'
                  modify $ M.insert k (insertBy (comparing fst) (tk,v) tkvs)
                  return v

parse :: [Int] -> [[(Int,Int,Int)]]
parse (n:ns) = unfoldr toTriplets ts : parse ns'
    where (ts,ns') = splitAt (3*n) ns
          toTriplets (a:b:c:xs) = Just ((a,b,c),xs)
          toTriplets _ = Nothing
parse _ = []

pick :: [a] -> [(a,[a])]
pick xs = zip xs $ zipWith (++) (init (inits xs)) (tail (tails xs))

zombieM :: (a,b,c) -> c
zombieM (_,_,m) = m

segLookup :: (Ord t, Eq a) => t -> [(t, a)] -> Maybe a
segLookup t ((t1,v1):(t2,v2):_) | v1 == v2 && t1 <= t && t <= t2 = Just v1
segLookup t ((t0,v0):_) | t == t0 = Just v0
segLookup t (_:ts) = segLookup t ts
segLookup _ _ = Nothing

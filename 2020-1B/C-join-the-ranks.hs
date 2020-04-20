module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Semigroup
import Data.Sequence (Seq,ViewL(..),fromFunction,viewl,spanl)

default (Int)

main :: IO ()
main = interact $ unlines . concat . zipWith format [1..] .
                  map (uncurry solve) . pairs . map read . tail . words
  where pairs (a:b:xs) = (a,b) : pairs xs
        pairs [] = []
        format n wss = ("Case #" ++ show n ++ ": " ++ show (length wss))
                     : map pair wss
        pair (i,j) = show i ++ " " ++ show j

solve :: Int -> Int -> [(Int,Int)]
solve r s = go (stimes s (fromFunction r succ))
  where go deck = case seekSplit deck of
          Nothing -> []
          Just (a,b,st) -> (length a,length b) : go ((b <> a) <> st)

seekSplit :: Seq Int -> Maybe (Seq Int,Seq Int,Seq Int)
seekSplit d = doubleJoin <|> singleJoin where
  ta :< _ = viewl d
  (a1,a') = spanl (== ta) d

  -- split A[ta,ba] B[tb..ta] S[ba..] -> B[tb..ta] A[ta,ba] S[ba..]
  doubleJoin = do let ba :< _ = viewl a'
                      (a2,b ) = spanl (== ba) a'
                      (b1,b') = spanl (/= ta) b
                      (b2,st) = spanl (== ta) b'
                  guard (not (null st))
                  pure (a1 <> a2,b1 <> b2,st)

  -- split A[a] B[tb..bb] S[a..] -> B[tb..bb] A[a] S[a..]
  singleJoin = do let (b,st) = spanl (/= ta) a'
                  guard (not (null st))
                  pure (a1,b,st)

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Monad (foldM,replicateM_,when,void)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Q
import System.Exit (exitSuccess)
import System.IO (hFlush,stdout)

query :: [Int] -> IO Int
query xs = do
  putStrLn $ unwords $ map show xs
  hFlush stdout
  r <- readLn
  when (r < 0) exitSuccess
  pure r

newtype Rank = Rank Int deriving (Eq,Ord,Enum,Num,Real,Integral)
newtype Index = Index Int deriving (Enum)
type Ranking = Seq Index

insertAt :: Rank -> Index -> Ranking -> Ranking
insertAt (Rank i) e q = Q.insertAt i e q

index :: Ranking -> Rank -> Index
index q (Rank r) = Q.index q r

firstRank,lastRank :: Ranking -> Rank
firstRank _ = Rank 0
lastRank q = Rank (Q.length q - 1)

submit :: Ranking -> IO ()
submit = void . query . coerce . toList

data Trisection = Before | Between | After

medianOrder :: Ranking -> Rank -> Rank -> Index -> IO Trisection
medianOrder q a b (Index i) = do
  let Index sa = index q a
      Index sb = index q b
  m <- query [sa,sb,i]
  if | m == sa -> pure Before
     | m == sb -> pure After
     | m ==  i -> pure Between

insertionSort :: Int -> IO Ranking
insertionSort n = foldM insert q0 [Index 3..Index n]
  where q0 = Q.fromList [Index 1,Index 2]
        insert q i = do
          p <- ternarySearch q i
          pure (insertAt p i q)

ternarySearch :: Seq Index -> Index -> IO Rank
ternarySearch q i = ternary (firstRank q) (succ (lastRank q)) where

  -- seek an insertion position in [l,r]
  ternary :: Rank -> Rank -> IO Rank
  ternary l r
    | w >= 2 = do
        let a = l + w     `div` 3
            b = l + w * 2 `div` 3
        medianOrder q a b i >>= \case
          Before  -> ternary  l    a
          After   -> ternary (b+1) r
          Between -> ternary (a+1) b
    | w == 0 = pure l
    | otherwise = binary l
    where w = r - l

  -- seek an insertion position among x and x+1
  binary :: Rank -> IO Rank
  binary x = do
    let a = firstRank q
        b = if x == a then lastRank q else x
    medianOrder q a b i >>= \case
      Before  -> pure a
      Between -> pure $ if x == a then (a+1) else b
      After   -> pure (b+1)

main :: IO ()
main = do
  [t,n,_] <- map read . words <$> getLine
  replicateM_ t (insertionSort n >>= submit)

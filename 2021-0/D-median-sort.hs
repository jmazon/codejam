{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Monad
import Data.Foldable
import qualified Data.Sequence as Q
import System.Exit
import System.IO

query :: [Int] -> IO Int
query xs = do
  putStrLn $ unwords $ map show xs
  hFlush stdout
  r <- readLn
  when (r < 0) exitSuccess
  pure r

main :: IO ()
main = do
  [t,n,_] <- map read . words <$> getLine
  replicateM_ t (medianSort n)
  
medianSort :: Int -> IO ()
medianSort n = do
  xs <- foldM insert (Q.fromList [1,2]) [3..n]
  void $ query $ toList xs

insert :: Q.Seq Int -> Int -> IO (Q.Seq Int)
insert q x = go 0 (Q.length q - 1) where
  go l r | l > r = pure (Q.insertAt l x q)
         | l == r && l > 0 = do
             let z = Q.index q 0
                 a = Q.index q l
             m <- query [a,z,x]
             if | m == z -> error "WTF"
                | m == x -> pure (Q.insertAt l x q)
                | m == a -> pure (Q.insertAt (l+1) x q)
         | l == r && l == 0 = do
             let _ Q.:|> b = q
                 a Q.:<| _ = q
             m <- query [a,b,x]
             if | m == a -> pure (Q.insertAt 0 x q)
                | m == x -> pure (Q.insertAt 1 x q)
                | m == b -> error "WTF"
         | otherwise = do
             let pa = l + (r - l) `div` 3
                 pb = l + (2 * (r - l + 1)) `div` 3
                 a = Q.index q pa
                 b = Q.index q pb
             m <- query [a,b,x]
             if | m == a -> go l (pa-1)
                | m == b -> go (pb+1) r
                | m == x -> go (pa+1) (pb-1)

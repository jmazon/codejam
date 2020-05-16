module Main (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as B

readInteger :: B.ByteString -> Integer
readInteger b = i where Just (i,_) = B.readInteger b

main :: IO ()
main = do
  t <- readInteger <$> B.getLine
  forM_ [1..t] $ \testNumber -> do
    [l,r] <- map readInteger . B.words <$> B.getLine
    let (n,l',r') = phase1 l r
    putStrLn $ "Case #" ++ show testNumber ++ ": " ++ unwords (map show [n,l',r'])

routine :: Integer -> Integer -> Integer -> (Integer,Integer)
routine i0 step stock = (n,f n) where
  n = bsearch ((<= stock) . f) 0 1000000000000000001
  f x = x * (2*i0 + step*(x-1)) `div` 2

bsearch :: (Integer -> Bool) -> Integer -> Integer -> Integer
bsearch pr = go where go a b | b-a <= 1 = a
                             | pr m = go m b
                             | otherwise = go a m
                        where m = (a + b) `div` 2

phase1 :: Integer -> Integer -> (Integer,Integer,Integer)
phase1 l r | l >= r    = let (n,q) = routine 1 1 (l - r) in phase2 n (l - q) r
           | otherwise = let (n,q) = routine 1 1 (r - l) in phase2 n l (r - q)

phase2 :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
phase2 n l r = (n + nl + nr,l - ql,r - qr) where
  ((nl,ql),(nr,qr)) | l >= r    = (routine (n+1) 2 l,routine (n+2) 2 r)
                    | otherwise = (routine (n+2) 2 l,routine (n+1) 2 r)

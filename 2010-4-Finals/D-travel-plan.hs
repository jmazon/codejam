{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Functor ((<$>))
import Data.List (delete,foldl',sort,genericIndex,genericLength)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Bifunctor (first)
import Control.Monad (forM_,guard,void,when,liftM2)
import System.IO
import Control.Parallel.Strategies

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine :: IO Int
  forM_ [1..t] $ \i -> do
    void getLine
    xs <- map read . words <$> getLine
    f <- read <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show' (large xs f)

small :: [Integer] -> Integer -> Maybe Integer
small (0:xs) f = case ps of
                   [] -> Nothing
                   _  -> Just (maximum ps)
    where ps = filter (<= f) $ map cost $ paths xs

paths :: Eq a => [a] -> [[a]]
paths [] = [[]]
paths xs = do
  x <- xs
  xs' <- paths (delete x xs)
  return (x : xs')

cost :: [Integer] -> Integer
cost = uncurry (+) . first abs . foldl' f (0,0)
    where f (p,s) c = (c, s + abs (c-p))

show' :: Show a => Maybe a -> String
show' Nothing = "NO SOLUTION"
show' (Just a) = show a

tests :: ([Integer] -> Integer -> Integer) -> IO ()
tests f = mapM_ (print . uncurry f) [ ([0,10,-10],40)
                                    , ([0,1,2,3,4],13)
                                    , ([0,1,2,3,4],7) ]

intervals :: Num a => [a] -> [a]
intervals (x:y:xs) = (y - x) : intervals (y:xs)
intervals _ = []

medium :: [Integer] -> Integer -> Maybe Integer
medium xs f = maximum $ Nothing : (Just <$> cs')
    where 
      is = (2*) <$> intervals (sort xs)
      ps = (1 :) <$> passes True (genericLength is - 1) 1
      cs = map (sum . zipWith (*) is) ps
      cs' = filter (<= f) cs

passes :: Bool -> Integer -> Integer -> [[Integer]]
passes _ 0 _ = return []
passes t n p = do
  p' <- (p+) <$> [-1..1]
  guard (p' > 0)
  when t $ guard (p' <= n)
  (p':) <$> passes t (n-1) p'

{-# INLINE passesV #-}
passesV :: Bool -> Int -> Int -> [V.Vector Int]
passesV t n p = filter (if t then convergesV else const True)
                $ filter (V.all (>0)) 
                $ map (V.scanl' (+) p)
                $ V.replicateM (n-1) [-1..1]

{-# INLINE convergesV #-}
convergesV :: V.Vector Int -> Bool
convergesV v = V.and $ V.imap f v
    where f i e = e <= V.length v - i

large :: [Integer] -> Integer -> Maybe Integer
large xs f = (f-) <$> foldl' large' Nothing (passesV False l 1)
    where 
      l = V.length is - r
      r = V.length is `div` 2

      is, isl, isr :: V.Vector Integer
      is = V.fromList $ (2*) `map` intervals (sort xs)
      isl = V.take l is
      isr = V.drop l is

      bs, bs', bs'' :: [S.Set Integer]
      bs = map (S.fromList 
                . map (V.sum . V.zipWith (*) isr)
                . map (V.map fromIntegral)
                . passesV True r) 
           [1 .. l]
      bs' = zipWith S.union bs (tail bs ++ [S.empty])
      bs'' = zipWith S.union bs' (S.empty : bs)

      large' :: Maybe Integer -> V.Vector Int -> Maybe Integer
      large' a p | isNothing a = b
                 | isNothing b = a
                 | otherwise   = liftM2 min a b `using` rdeepseq
          where 
            b = if S.null s'
                then Nothing
                else Just $ f' - S.findMax s'
            s' = fst $ S.split (f'+1) s

            f' :: Integer
            f' = f - V.sum (V.zipWith (*) isl (V.map fromIntegral p))

            s = genericIndex bs'' . pred . V.last $ p

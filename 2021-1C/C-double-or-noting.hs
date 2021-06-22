import Debug.Trace

import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.List
import Data.Maybe

import qualified Data.IntMap.Strict as M
import Data.Word

readI :: Bits i => String -> i
readI = foldl' (\ a b -> (if b == '1' then setBit else clearBit) (a `shiftL` 1) 0) zeroBits

brute :: String -> String -> Maybe Int
brute s e = go (M.singleton (readI e) 0) where
  go = listToMaybe . mapMaybe (M.lookup (readI s)) . take 256 . iterate (\m -> foldl' expand m [0..255])
  expand :: M.IntMap Int -> Int -> M.IntMap Int
  expand m i = notIt i $ doubleIt i m
  notIt,doubleIt :: Int -> M.IntMap Int -> M.IntMap Int
  notIt i m =
    let mask | testBit i 7 = 255
             | testBit i 6 = 63
             | testBit i 5 = 31
             | testBit i 4 = 15
             | testBit i 3 = 7
             | testBit i 2 = 3
             | otherwise = 1 -- covers 1 <-> 0
    in maybe m (\d -> M.insertWith min i (d+1) m) $ M.lookup (complement i .&. mask) m
  doubleIt i m | testBit i 7 = m
               | otherwise = maybe m (\d -> M.insertWith min i (d+1) m) $ M.lookup (i `shiftL` 1) m
    

data Extensible = Never | AfterNot | Always deriving (Eq,Show)

afterNot :: Extensible -> Extensible
afterNot AfterNot = Always
afterNot x = x

-- | (digit sequence length,is sequence extensible?)
newtype I = I [(Int,Extensible)] deriving Show

rd :: String -> I
rd = intsToI . map length . group . dropWhile (== '0')

intsToI :: [Int] -> I
intsToI = I . go where
  go (a:b:xs) = [(a,Never),(b,if null xs then Always else Never)] ++ go xs
  go [x] = [(x,AfterNot)]
  go [] = []

iToInts :: I -> [Int]
iToInts (I xs) = map fst xs

iNot :: I -> I
iNot (I xs) = I $ map (second afterNot) $ tail xs

iNots :: I -> [I]
iNots = unfoldr f where
  f (I []) = Nothing
  f i = Just (i,iNot i)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [s,e] <- words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++
      maybe "IMPOSSIBLE" show (brute s e)

minimum' :: Ord a => [a] -> Maybe a
minimum' [] = Nothing
minimum' xs = Just (minimum xs)

sum' :: [(Int,Extensible)] -> Int
sum' = sum . map fst

solve :: I -> I -> Maybe Int
solve (I []) (I []) = Just 0
solve (I []) (I [(1,AfterNot)]) = Just 1
solve s0 (I e0) = minimum' $ catMaybes $ zipWith (fmap . (+)) [0..] $ map (attempt e0) $ iNots s0 where
  attempt e (I []) = Just (sum' e)
  attempt ((x,_):e') (I ((y,ext):s'))
    | y == x = attempt e' (I s')
    | ext == Always && y <= x = (+ (x-y)) <$> attempt e' (I s')
  attempt _ _ = Nothing

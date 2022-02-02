{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
import Control.Applicative
import Control.Monad
import Data.List
import Data.Semigroup

import qualified Data.Set as Set

import Test.QuickCheck
import Data.Word

import Debug.Trace

tr = id
-- tr = traceShowId

readI :: String -> [Int]
readI "0" = []
readI s = map length (group s)

main :: IO ()
main = do
  quickCheck prop_brute
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [s,e] <- map readI . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++
      maybe "IMPOSSIBLE" show (solve s e)

solve :: [Int] -> [Int] -> Maybe (Min Int)
solve s0 [] = Just (Min (length s0))
solve s0 e0 = go (zip s0 (cycle [True,False])) 0 where
  e0' = zip e0 (cycle [True,False])

  go :: [(Int,Bool)] -> Int -> Maybe (Min Int)
  go s@(_:s') nots0 = tr (cmp s e0' nots0) <> go s' (nots0 + 1)
  go [] nots0 = tr $ case e0 of
    [] -> pure (Min 0)    -- ∅
    [1] -> pure (Min 1)   -- NOT
    [a] -> pure (Min (a + 1 + fromEnum (nots0 == 0))) -- [NOT], double^a, NOT
    [1,b] -> pure (Min (b + fromEnum (nots0 == 0))) -- [NOT], double^b
    [a,b] -> pure (Min (a + b + 1 + fromEnum (nots0 == 0))) -- [NOT], double^a, NOT, double^b
    _ -> empty
    
--  cmp :: a -> b -> c -> Maybe (Min Int)
  -- suffix match requires too many NOTs
  cmp _ _ nots | nots < 0 = empty
  -- one-character head match → advance
  cmp ((hs,_):ts) ((he,_):te) nots | hs == he = cmp ts te nots
  -- extensible tail match → extend and advance
  cmp [(hs,one)] ((he,_):te) nots | hs < he && (not one || nots > 0) =
    fmap (he - hs +) <$> cmp [] te (nots - 1)
  -- reached both ends → it's a match
  cmp [] [] nots = pure (Min nots)
  -- at S end → fail on an inextensible odd position
  cmp [] ((_,True):te) nots | nots <= 0 = empty
  -- reached S end → count remaining NOTs and doublings
  cmp [] e nots =
    let nots' = length e
        doubles = sum (fst <$> e)
    in fmap (nots' + doubles +) <$> cmp [] [] (nots - nots')
  -- unfixable mixmatch → fail
  cmp _ _ _ = empty

samples :: [Bool]
samples =
  [ solve [1,3,1] [3] == Just 4
  , solve [1,1,2] [3] == Just 3
  , solve [1,1,1,1] [1,1,2] == Just 2
  , solve [] [1] == Just 1
  , solve [] [1,1,1] == Nothing
  , solve [2,1,1,1,2] [2,1,1,1,2] == Just 0
  ]

doubleI :: [Int] -> [Int]
doubleI [] = []
doubleI ns0 = go True ns0 where
  go False [] = [1]
  go False [n] = [n+1]
  go one (n:ns) = n : go (not one) ns

notI :: [Int] -> [Int]
notI [] = [1]
notI (_:is) = is

newtype DList x = DList ([x] -> [x])

dlFromList :: [x] -> DList x
dlFromList = DList . (++)

dlToList :: DList x -> [x]
dlToList (DList apply) = apply []

dlSingleton :: x -> DList x
dlSingleton = DList . (:)

dlAppend :: DList x -> DList x -> DList x
dlAppend (DList xs) (DList ys) = DList (xs . ys)

pattern Nil :: DList x
pattern Nil <- (dlToList -> [])

pattern Cons :: x -> [x] -> DList x
pattern Cons x xs <- (dlToList -> (x:xs))

{-# COMPLETE Nil,Cons #-}

data Action = Double | Not deriving Show

brute :: [Int] -> [Int] -> Maybe [Action]
brute s e = bfs Set.empty (dlSingleton (s,[],0)) where
  bfs :: Set.Set [Int] -> (DList ([Int],[Action],Int)) -> Maybe [Action]
  bfs _ Nil = Nothing
  bfs cl (Cons (n,p,d) q)
    | n == e = pure p
    | n `Set.member` cl || d >= 200 = bfs cl (dlFromList q)
    | otherwise = bfs cl' (dlFromList q `dlAppend` dlFromList (filter ((`Set.notMember` cl) . fst3) [(doubleI n,Double:p,d+1),(notI n,Not:p,d+1)]))
      where cl' = Set.insert n cl
            
prop_brute :: [Word8] -> [Word8] -> Property
prop_brute s e = within 10000000 $ solve s' e' == fmap (Min . length) (brute s' e')
  where s' = succ . fromIntegral <$> s
        e' = succ . fromIntegral <$> e

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

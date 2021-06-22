{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Functor ((<$>))
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Text.Read
import System.IO
import Control.Parallel.Strategies

-- trivial solution: for each letter, push it, print it, pop it
-- with a maximum of 7000 letters, that makes an easy upper bound
inf :: Int
inf = 21000

-- there's always an optimal solution where the stack is constantly of
-- the form xyzxyzxy...
stackBases :: [ V.Vector Char ]
stackBases = map V.fromList [ "ABC", "ACB", "BAC", "BCA", "CAB", "CBA" ]

-- state is the number of operations needed to solve the remaining
-- characters, for each stack base and current stack length

-- final state (all characters processed):
-- as many operations to solve as stamps left in the stack (pop them!)
final :: Int -> [V.Vector Int]
final n = replicate 6 $ V.enumFromTo 0 n

-- backpropagate: for a given stack base, express the cost of
-- prefixing c in terms of the cost to solve the remainder
back :: Char -> V.Vector Char -> V.Vector Int -> V.Vector Int
back c base next = V.create $ do
  v <- MV.new (V.length next)
  V.forM_ (V.enumFromN 0 (V.length next)) $ \l ->
    if topIs base l c
    then MV.write v l $ 1 + next!l -- print
    else if top'Is base l c
    then MV.write v l $ 2 + next!(l-1) -- pop/print
    else do
      let ph' = if pushIs base l == c && l+1 < V.length next -- push/print
                then 2 + next!(l+1) 
                else inf
      pp <- if l > 0 then MV.read v (l-1) else return inf -- pop and recurse
      MV.write v l $ min ph' (pp+1)
  return v
{-# INLINE back #-}

-- backpropagate for all stack bases, and merge stacks of length
-- 0 and 1: in that situation, the stack base can change mid-process
back' :: Char -> [V.Vector Int] -> [V.Vector Int]
back' c nexts = zipWith smoothen stackBases curs `using` rdeepseq
    where curs = zipWith (back c) stackBases nexts `using` rdeepseq
          min0 = minimum $ map V.head curs
          minA = min (curs!!0!1) (curs!!1!1)
          minB = min (curs!!2!1) (curs!!3!1)
          minC = min (curs!!4!1) (curs!!5!1)
          smoothen base cur = V.modify (\m -> do
            MV.write m 0 min0
            MV.write m 1 $ case V.head base of
                             'A' -> minA
                             'B' -> minB
                             'C' -> minC)
                              cur

-- solving: add characters one by one.
solve :: V.Vector Char -> Int
solve q = V.head $ head $ V.foldr' back' (final (V.length q)) q

-- stack examination
topIs :: V.Vector Char -> Int -> Char -> Bool
topIs base l c = l >= 1 && base ! (mod (l-1) 3) == c
{-# INLINE topIs #-}

top'Is :: V.Vector Char -> Int -> Char -> Bool
top'Is base l c = l >= 2 && base ! (mod (l-2) 3) == c
{-# INLINE top'Is #-}

pushIs :: V.Vector Char -> Int -> Char
pushIs base l = base ! (mod l 3)
{-# INLINE pushIs #-}

-- boilerplate
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  t <- Text.Read.read <$> getLine :: IO Int
  V.forM_ (V.enumFromN 1 t) $ \i -> do
    q <- V.fromList <$> getLine
    putStrLn $ "Case #" ++ show (i :: Int) ++ ": " ++ show (solve q)

-- ~8min for a single result
-- -> a bit too slow yet

-- 2.2G alloc, 25"
-- INLINE back -> 2.2G alloc, 21.5"
-- rdeepseq -> 2.2G alloc, 20.7"

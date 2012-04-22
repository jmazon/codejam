import Data.Functor ((<$>))
import Data.Vector.Unboxed hiding ((++))
import Data.Vector.Unboxed.Mutable hiding (length)
import qualified Data.List.Stream as L
import qualified Text.Read
import System.IO
import Prelude hiding (enumFromTo,length,head,read)
import Control.Parallel.Strategies

instance Unbox a => NFData (Vector a) where rnf = foldr' seq ()

-- trivial solution: for each letter, push it, print it, pop it
-- with a maximum of 7000 letters, that makes an easy upper bound
inf = 21000

-- there's always an optimal solution where the stack is constantly of
-- the form xyzxyzxy...
stackBases = L.map fromList [ "ABC", "ACB", "BAC", "BCA", "CAB", "CBA" ]

-- state is the number of operations needed to solve the remaining
-- characters, for each stack base and current stack length

-- final state (all characters processed):
-- as many operations to solve as stamps left in the stack (pop them!)
final n = L.replicate 6 $ enumFromTo 0 n

-- backpropagate: for a given stack base, express the cost of
-- prefixing c in terms of the cost to solve the remainder
back c base next = create $ do
  v <- new (length next)
  forM_ (enumFromN 0 (length next)) $ \l ->
    if topIs base l c
    then write v l $ 1 + next!l -- print
    else if top'Is base l c
    then write v l $ 2 + next!(l-1) -- pop/print
    else do
      let ph' = if pushIs base l == c && l+1 < length next -- push/print
                then 2 + next!(l+1) 
                else inf
      pp <- if l > 0 then read v (l-1) else return inf -- pop and recurse
      write v l $ min ph' (pp+1)
  return v
{-# INLINE back #-}

-- backpropagate for all stack bases, and merge stacks of length
-- 0 and 1: in that situation, the stack base can change mid-process
back' c nexts = L.zipWith smoothen stackBases curs `using` rdeepseq
    where curs = L.zipWith (back c) stackBases nexts `using` rdeepseq
          min0 = L.minimum $ L.map head curs
          minA = min (curs!!0!1) (curs!!1!1)
          minB = min (curs!!2!1) (curs!!3!1)
          minC = min (curs!!4!1) (curs!!5!1)
          smoothen base cur = flip modify cur $ \c -> do
            write c 0 min0
            write c 1 $ case head base of
                          'A' -> minA
                          'B' -> minB
                          'C' -> minC

-- solving: add characters one by one.
solve seq = head $ L.head $
            foldr' back' (final (length seq)) seq

-- stack examination
topIs base l c = l >= 1 && base ! (mod (l-1) 3) == c
{-# INLINE topIs #-}
top'Is base l c = l >= 2 && base ! (mod (l-2) 3) == c
{-# INLINE top'Is #-}
pushIs base l = base ! (mod l 3)
{-# INLINE pushIs #-}

-- boilerplate
main = do
  hSetBuffering stdout LineBuffering
  t <- Text.Read.read <$> getLine :: IO Int
  forM_ (enumFromN 1 t :: Vector Int) $ \t -> do
    seq <- fromList <$> getLine
    putStrLn $ "Case #" ++ show t ++ ": " ++ show (solve seq)

-- ~8min for a single result
-- -> a bit too slow yet

-- 2.2G alloc, 25"
-- INLINE back -> 2.2G alloc, 21.5"
-- rdeepseq -> 2.2G alloc, 20.7"

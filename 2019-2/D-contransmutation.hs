module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Semigroup
import Data.Graph
import Data.Array.ST
import qualified Data.IntSet as S
import qualified Data.ByteString.Char8 as B

-- Qty: Null to distinguish 0 from BigPrime; Unbounded to compact
data Qty = Null | Finite !Int | Unbounded deriving (Eq,Ord)
instance Show Qty where show Null = "0"
                        show (Finite i) = show i
                        show Unbounded = "UNBOUNDED"
toQty :: Int -> Qty
toQty 0 = Null
toQty n = Finite n
-- this is ugly because @#%$^& obsolete GHC version at Google's
instance Semigroup Qty where
  Null     <> x        = x
  x        <> Null     = x
  Finite i <> Finite j = Finite ((i+j) `mod` 1000000007)
  _        <> _        = Unbounded
instance Monoid Qty where mappend = (Data.Semigroup.<>)
                          mempty = Null

mkEdges :: Int -> [Int] -> ((),Int,[Int])
mkEdges src dsts = ((),src,dsts)

getIntLine :: IO [Int]
getIntLine = map readInt . B.words <$> B.getLine
  where readInt b = let Just (i,_) = B.readInt b in i

main :: IO ()
main = do
  [t] <- getIntLine
  forM_ [1..t] $ \i -> do
    [nm] <- getIntLine
    nodes <- zipWith mkEdges [1..] <$> replicateM nm getIntLine
    treasury <- map toQty <$> getIntLine

    let (g,nodeFromVertex) = graphFromEdges' nodes
        lead = runST $ do
          tr <- newListArray (1,nm) treasury :: ST s (STArray s Int Qty)

          -- for each strongly connected component
          forM_ (map (fmap nodeFromVertex) (scc (transposeG g))) $ \cmp -> do
            let metals = foldMap (\(_,k,_) -> S.singleton k) cmp
                size = S.size metals
                outDegree = sum $ fmap (\(_,_,o) -> length $ filter (`S.notMember` metals) o) cmp
            weight <- foldlM (\w m -> mappend w <$> readArray tr m)
                             mempty (S.elems metals)
            let hasWeight = weight > Null
                hasLoop = size > 1 || outDegree <= 1

            -- within SCC: UB if fewer exits than nodes
            let cmpWeight | hasWeight && outDegree < size = Unbounded
                          | otherwise = weight

            -- SCC targets: UB if there's a loop
            let prop | hasWeight && hasLoop = Unbounded
                     | otherwise = cmpWeight

            -- assign and propagate
            forM_ cmp $ \(_,src,dsts) -> do
              writeArray tr src cmpWeight
              forM_ dsts $ \dst -> when (dst `S.notMember` metals) $
                writeArray tr dst . (mappend prop) =<< readArray tr dst

          readArray tr 1

    putStrLn $ "Case #" ++ show i ++ ": " ++ show lead

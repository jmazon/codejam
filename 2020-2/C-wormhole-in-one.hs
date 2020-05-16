module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S

readInt :: B.ByteString -> Int
readInt b = i where Just (i,_) = B.readInt b

data Point = P !Int !Int
readPoint :: B.ByteString -> Point
readPoint b = P x y where [x,y] = map readInt (B.words b)

newtype Dir = Dir (Int,Int) deriving (Eq,Ord)
angle :: Point -> Point -> Dir
angle (P x1 y1) (P x2 y2)
    | den > 0 || den == 0 && num >= 0 = Dir ( num, den)
    | otherwise                       = Dir (-num,-den)
  where g = gcd (y2-y1) (x2-x1)
        num = (y2-y1) `div` g
        den = (x2-x1) `div` g

angleHoles :: Int -> S.Set (Int,Int) -> Int
angleHoles n ps = runST $ do
  uf <- ufNew n
  mapM_ (uncurry (ufJoin uf)) (S.elems ps)
  (evens,odds) <- partition even . filter (> 1) .
                  map length . group . sort <$>
                  mapM (ufRep uf) [1..n]
  pure $ min n (sum evens + sum odds + if even (length odds) then 2 else 1)

main :: IO ()
main = do
  t <- readInt <$> B.getLine
  forM_ [1..t] $ \testNumber -> do
    n <- readInt <$> B.getLine
    ps <- listArray (1,n) . map readPoint <$> replicateM n B.getLine
    let anglePairs = M.fromListWith S.union $ do
          (p:ns) <- tails [1..n]
          p' <- ns
          pure (angle (ps!p) (ps!p'),S.singleton (p,p'))
        answer | M.null anglePairs = 1
               | otherwise = maximum (angleHoles n <$> anglePairs)
    putStrLn $ "Case #" ++ show testNumber ++ ": " ++ show answer

-- It's only 100 points, not bothering with union sizes…
type UF s = STUArray s Int Int

ufNew :: Int -> ST s (UF s)
ufNew n = newListArray (1,n) [1..n]

ufJoin :: UF s -> Int -> Int -> ST s ()
ufJoin uf a b = do
  ra <- ufRep uf a
  rb <- ufRep uf b
  writeArray uf (max ra rb) (min ra rb)

ufRep :: UF s -> Int -> ST s Int
ufRep uf i = do
  p <- readArray uf i
  if p == i then pure i else do
    r <- ufRep uf p
    writeArray uf i r -- but this optimization is too
    pure r            -- easy to implement to pass on

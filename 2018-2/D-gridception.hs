{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Array
import Data.Array.ST
import Data.Graph
import Data.List
import Data.Bool
import Control.Applicative
import Control.Monad
import Control.Monad.ST

default (Int)

small :: Array (Int,Int) Bool -> Int
small = seek where
  seek grid = maximum $ do
    let bds' = bounds grid
    pat <- patterns
    (di,dj) <- range ((0,0),(11,15))
    guard $ all ( \(i,j) -> let (i',j') = ( (i+di-1) `div` 4 + 1
                                          , (j+dj-1) `div` 4 + 1) in
                            inRange bds' (i,j) &&
                            inRange bds' (i',j') &&
                            grid!(i',j') == grid!(i,j) ) pat
    pure (length pat)

  bds = ((1,1),(3,4))
  patterns = filter connected $ filterM (const [False,True]) $ range bds
  dist (a,b) (c,d) = abs (c-a) + abs (d-b)
  connected ps = runST $ do
    uf <- thaw uf0 :: ST s (STArray s (Int,Int) (Int,Int))
    sequence_ (liftA2 (connect uf) ps ps)
    fmap (null . drop 1 . group) $ mapM (rep uf) ps
  connect uf a b = when (dist a b == 1) $ do
    ra <- rep uf a
    rb <- rep uf b
    writeArray uf (max ra rb) (min ra rb)
  uf0 = listArray bds (range bds)
  rep uf i = do p <- readArray uf i
                if p == i then pure i else rep uf p

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p x = bool empty (pure x) (p x)

--large :: Array (Int,Int) Bool -> Int
large grid = maximum $ do
  let bds@(lo,hi) = bounds grid
  (sI,sJ) <- indices grid
  let q = (map.map) (fmap (grid!) . guarded (inRange bds)) [[(sI-1,sJ-1),(sI-1,sJ)],[(sI,sJ-1),(sI,sJ)]]
  sel <- (replicateM 2 . replicateM 2) [False,True]
  let at (i,j) = sel !! fromEnum (i < sI) !! fromEnum (i < sJ) $
                 grid ! (i,j)
      graph = buildG (index bds lo,index bds hi)
                     [ (index bds (i,j),index bds n)
                     | (i,j) <- range bds, at (i,j)
                     , n <- [ (i-1,j),(i+1,j),(i,j-1),(i,j+1) ]
                     , inRange bds n, at n ]
  fmap (,(sI,sJ)) $
   fmap length $ dff graph

check :: (Show t,Show a,Eq t) => (a -> t) -> (a -> t) -> a -> t
check f g x | r == r' = r
            | otherwise = error $ "Mismatch on " ++ show x ++ ": " ++ show (r,r')
  where r = f x
        r' = g x

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \tn -> do
    [r,c] <- map read . words <$> getLine
    grid <- listArray ((1,1),(r,c)) . map (== 'W') . concat <$>
            replicateM r getLine
    putStrLn $ "Case #" ++ show tn ++ ": " ++ show 42--(check small large grid)

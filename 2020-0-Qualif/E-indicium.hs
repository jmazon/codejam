{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad
import Data.Array
import Data.Array.ST
import Data.List
import System.Environment

import Data.List.Split
import Control.Monad.State

-- for Data.Graph.MaxBipartiteMatching
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List ( foldl' )

trace :: [[Int]] -> Int
trace xss = sum $ zipWith (!!) xss [0..]

main :: IO ()
main = do
  getArgs >>= \case
    ["--generate"] -> do
      print $ M.fromList $ do
        n <- [2..5]
        square <- genSmalls n
        pure ((n,trace square),square)
    _ -> do
      t <- readLn
      forM_ [1..t] $ \i -> do
        [n,k] <- map read . words <$> getLine
        putStr $ "Case #" ++ show i ++ ": "
        case (if n <= 5 then solveSmall else solveLarge) n k of
          Nothing -> putStrLn "IMPOSSIBLE"
          Just s -> putStrLn "POSSIBLE" *>
                    mapM_ (putStrLn . unwords . map show) s

solveSmall, solveLarge :: Int -> Int -> Maybe [[Int]]

solveSmall n k = M.lookup (n,k) smallSquares

solveLarge n k = do
  let abcs | n == 2 = guard (even k) *> [ (undefined,k `div` 2,k `div` 2 ) ]
           | n == 3 = let (q,r) = k `divMod` 3 in guard (r == 0) *> pure (q,q,q)
           | otherwise = [ (a,b,c)
                         | a <- [1..n], b <- [1..n], let c = k - (n-2)*a - b
                         , inRange (1,n) c, (a == b) == (a == c) ]
  case abcs of
    [] -> Nothing
    ((a,b,c):_) -> do
      let diag 1 = c
          diag 2 = b
          diag _ = a
          m = runSTArray $ do
            m <- newArray ((1,1),(n,n)) 0
            forM_ [1..n] $ \i -> writeArray m (i,i) (diag i)
            forM_ [1..n] $ \i -> do
              es <- forM (delete i [1..n]) $ \j -> do
                vs <- mapM (\i' -> readArray m (i',j)) [1..n]
                pure [ (v,j) | v <- [1..n] \\ (diag i : diag j : vs) ]
              let match = matching (S.fromList (concat es))
              forM_ (M.assocs match) $ \(j,v) -> writeArray m (i,j) v
            pure m
      Just (chunksOf n (elems m))

genSmalls :: Int -> [[[Int]]]
genSmalls n = go (n-1) . pure =<< rotations [1..n] where
  go 0 ls = pure ls
  go i ls = do
    let cols = transpose ls
    l' <- mapAccumM' (\vbd col -> [1..n] \\ (vbd ++ col) >>= \c ->
                       pure (c,c:vbd)) [] cols
    go (i-1) (l' : ls)

rotations :: [a] -> [[a]]
rotations xs = zipWith (flip const) xs $
               map (zipWith (flip const) xs) $
               tails (xs ++ xs)

mapAccumM' :: (Traversable t,Monad m)
          => (a -> b -> m (c,a)) -> a -> t b -> m (t c)
mapAccumM' f = flip (evalStateT . (traverse (StateT . (flip f))))

smallSquares :: M.Map (Int,Int) [[Int]]
smallSquares = M.fromList [((2,2),[[1,2],[2,1]]),((2,4),[[2,1],[1,2]]),((3,3),[[1,2,3],[3,1,2],[2,3,1]]),((3,6),[[1,2,3],[2,3,1],[3,1,2]]),((3,9),[[3,1,2],[2,3,1],[1,2,3]]),((4,4),[[1,4,3,2],[3,1,2,4],[4,2,1,3],[2,3,4,1]]),((4,6),[[1,2,3,4],[2,1,4,3],[4,3,2,1],[3,4,1,2]]),((4,7),[[1,4,3,2],[3,2,4,1],[2,3,1,4],[4,1,2,3]]),((4,8),[[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]]),((4,9),[[1,3,4,2],[2,4,3,1],[3,2,1,4],[4,1,2,3]]),((4,10),[[2,3,4,1],[1,4,3,2],[3,2,1,4],[4,1,2,3]]),((4,11),[[1,4,3,2],[2,3,1,4],[3,2,4,1],[4,1,2,3]]),((4,12),[[3,2,1,4],[1,3,4,2],[2,4,3,1],[4,1,2,3]]),((4,13),[[2,3,1,4],[1,4,3,2],[3,2,4,1],[4,1,2,3]]),((4,14),[[3,2,1,4],[1,4,3,2],[2,3,4,1],[4,1,2,3]]),((4,16),[[4,3,2,1],[2,4,1,3],[3,1,4,2],[1,2,3,4]]),((5,5),[[1,2,5,3,4],[3,1,2,4,5],[4,5,1,2,3],[5,4,3,1,2],[2,3,4,5,1]]),((5,7),[[1,5,2,3,4],[2,1,3,4,5],[4,2,1,5,3],[5,3,4,2,1],[3,4,5,1,2]]),((5,8),[[1,2,3,4,5],[2,1,5,3,4],[3,4,2,5,1],[5,3,4,1,2],[4,5,1,2,3]]),((5,9),[[1,3,4,2,5],[3,2,5,4,1],[2,4,1,5,3],[4,5,3,1,2],[5,1,2,3,4]]),((5,10),[[1,3,5,4,2],[3,2,4,1,5],[2,4,1,5,3],[4,5,3,2,1],[5,1,2,3,4]]),((5,11),[[1,2,5,4,3],[2,3,4,1,5],[3,4,1,5,2],[4,5,3,2,1],[5,1,2,3,4]]),((5,12),[[1,3,4,5,2],[2,4,5,1,3],[3,2,1,4,5],[4,5,3,2,1],[5,1,2,3,4]]),((5,13),[[3,2,4,1,5],[1,3,5,4,2],[2,4,1,5,3],[4,5,3,2,1],[5,1,2,3,4]]),((5,14),[[1,3,4,5,2],[3,2,1,4,5],[2,4,5,1,3],[4,5,3,2,1],[5,1,2,3,4]]),((5,15),[[1,2,4,5,3],[2,3,1,4,5],[3,4,5,1,2],[4,5,3,2,1],[5,1,2,3,4]]),((5,16),[[2,3,1,4,5],[3,4,5,1,2],[1,2,4,5,3],[4,5,3,2,1],[5,1,2,3,4]]),((5,17),[[3,2,1,4,5],[1,3,4,5,2],[2,4,5,1,3],[4,5,3,2,1],[5,1,2,3,4]]),((5,18),[[3,2,4,1,5],[2,4,1,5,3],[1,3,5,4,2],[4,5,3,2,1],[5,1,2,3,4]]),((5,19),[[1,5,4,2,3],[2,4,3,1,5],[3,2,5,4,1],[4,3,1,5,2],[5,1,2,3,4]]),((5,20),[[1,2,3,4,5],[2,5,4,1,3],[3,4,5,2,1],[4,3,1,5,2],[5,1,2,3,4]]),((5,21),[[2,4,3,1,5],[1,5,4,2,3],[3,2,5,4,1],[4,3,1,5,2],[5,1,2,3,4]]),((5,22),[[3,2,4,1,5],[2,5,3,4,1],[1,4,5,2,3],[4,3,1,5,2],[5,1,2,3,4]]),((5,23),[[4,2,3,1,5],[1,5,4,2,3],[2,3,5,4,1],[3,4,1,5,2],[5,1,2,3,4]]),((5,25),[[5,1,4,2,3],[2,5,1,3,4],[3,4,5,1,2],[4,3,2,5,1],[1,2,3,4,5]])]

-- Next two functions are Data.Graph.MaxBipartiteMatching
-- from hackage's maxBipartiteMatching-0.1.0.0

matching :: (Ord a, Ord b) => S.Set (a,b) -> M.Map b a
matching g = opt (M.keys fwd, []) fwd M.empty
    where
    fwd = foldl' (\m (x,y) -> M.insertWith (++) x [y] m) M.empty $ S.toList g

opt :: (Ord a, Ord b) => ([a],[a]) -> M.Map a [b] -> M.Map b a -> M.Map b a
opt (x:free,failed) fwd mat
  = either (flip (opt (free,x:failed)) mat) (opt (free++failed,[]) fwd)
    $ right fwd [] x
  where
    right rem path x
      = maybe (Left rem) (left $ M.delete x rem) $ M.lookup x rem
      where
        left rem [] = Left rem
        left rem (y:ys)
          = maybe
            (Right $ foldr (uncurry $ flip M.insert) mat path') --A
            (either (flip left ys) Right . right rem path')     --B
            (M.lookup y mat)                                    --C
          where
            path' = (x,y):path
opt ([],failed) fwd mat = mat

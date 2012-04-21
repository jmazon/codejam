import qualified Data.IntMap as M
import qualified Data.Set as S
import Data.List
import Data.Array.IArray
import Control.Monad
import Control.Applicative
import Control.Arrow
import System.IO

type Cell = Either Op Int
data Op = Plus | Minus

instance Show Op where
    show Plus = "+"
    show Minus = "-"

isDigit (Right _) = True
isDigit _ = False

fromOp (Left o) = o
fromDigit (Right d) = d

sign Plus = 1
sign Minus = -1

type Square = Array Pos Cell
type DSquare = Array Pos [(Pos, (Int, String))]

sample ::Square
sample = listArray ((1,1), (5,5))
         [ Right 2, Left Plus, Right 1, Left Minus, Right 2,
           Left Plus, Right 3, Left Minus, Right 4, Left Plus,
           Right 5, Left Plus, Right 2, Left Plus, Right 1,
           Left Minus, Right 4, Left Minus, Right 0, Left Minus,
           Right 9, Left Plus, Right 5, Left Plus, Right 1 ]

type Pos = (Int,Int)
type Workspace = Array Pos (M.IntMap String)
type Closed = S.Set (Pos,Int)

roots :: Square -> Workspace
roots s = array (bounds s) $ map f $ assocs s
    where f (c, Left _) = (c, M.empty)
          f (c, Right d) = (c, M.singleton d $ show d)

extensions :: Square -> Pos -> [(Pos, (Int, String))]
extensions s c = do
  c' <- neighbors s c
  c'' <- neighbors s c'
  let op = fromOp $ s!c'
      term = fromDigit $ s!c''
  return (c'', (term * sign op, show op ++ show term))

neighbors s (i,j) = filter (inRange (bounds s))
                    [ (i-1,j), (i+1,j), (i,j-1), (i,j+1) ]

derived :: Square -> DSquare
derived s = array (bounds s) . nub . sort . map f $ assocs s
    where f (p, v) | isDigit v = (p, extensions s p)
                   | otherwise = (p, [])

query :: Square -> [Int] -> M.IntMap String
query s qs = M.unions $
             query' (derived s)
                    (M.fromList . zip qs $ repeat ())
                    (roots s)
                    S.empty

query' :: DSquare -> M.IntMap () -> Workspace -> Closed -> [M.IntMap String]
query' s qs l c | M.null qs = []
                | otherwise = as : query' s qs' l' c'
    where as = M.intersection (M.unionsWith min (elems l)) qs
          qs' = M.difference qs as
          l' = accumArray g M.empty (bounds l) $ 
               filter (flip S.notMember c . noExpr) $ 
               concatMap f $ 
               assocs l
          f (p, m) = [ (p', (v+t, e++e')) |
                       (v, e) <- M.toList m,
                       (p', (t, e')) <- s!p ]
          g m (v, e) = M.insertWith min v e m
          noExpr (p,(v,_)) = (p,v)
          c' = foldl' (flip S.insert) c [ (p, v) | 
                                          (p,vs) <- assocs l'
                                        , v <- M.keys vs ]

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
         putStrLn $ "Case #" ++ show i ++ ":"
         [w,q] <- map read . words <$> getLine
         s <- readSqr w
         qs <- map read . words <$> getLine
         let as = query s qs
         mapM_ putStrLn $ map (as M.!) qs

readSqr :: Int -> IO Square
readSqr w = listArray ((1,1), (w,w)) .
            map toCell .
            concat <$>
            replicateM w getLine

toCell '+' = Left Plus
toCell '-' = Left Minus
toCell d = Right $ fromEnum d - fromEnum '0'
            
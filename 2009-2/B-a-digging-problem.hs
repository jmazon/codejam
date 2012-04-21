{-# LANGUAGE RankNTypes #-}

import Control.Arrow
import Data.MemoCombinators
import Data.Function
import qualified Data.Set as S
import qualified Data.Heap as H
import Data.Array
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import System.IO
--import System.IO.Unsafe

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    [r,c,f] <- map read . words <$> getLine
    cv <- cave r c <$> replicateM r getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show' (solveDP2 f cv)

cave r c xs = listArray ((1,1),(r,c)) . map f $ concat xs
    where f '#' = True
          f '.' = False

r (r,_,_,_) = r

type Cave = Array (Int,Int) Bool
type State = (Int,Int,Int,Cave)
type Node = (Int,State)

solveAstar f cv = astar goal (nextStates f) (H.singleton (0,(1,1,0,cv))) S.empty

goal (i,_,_,cv) = i == height cv

astar :: (State -> Bool) -> (State -> [Node]) -> H.MinHeap Node -> S.Set State -> Maybe Int
astar goal next open close | isNothing n = Nothing
                           | goal s = Just d
                           | S.member s close = astar goal next open' close
                           | otherwise = astar goal next open'' close'
    where n = H.view open
          (p@(_,s@(_,_,d,_)), open') = fromJust n
          children = next s
          open'' = foldl' (flip H.insert) open' children
          close' = S.insert s close

nextStates f s@(i,j,d,cv) = map makeNode $ 
                            catMaybes [
                             goLeft f s,
                             goRight f s,
                             digLeft s,
                             digRight s
                            ]

goLeft f (i,j,d,cv) = isClear cv (i,j-1) >> fall f cv (i,j-1,d)
goRight f (i,j,d,cv) = isClear cv (i,j+1) >> fall f cv (i,j+1,d)

isClear cv = guard . isClear' cv
isNotClear cv = guard . not . isClear' cv
isClear' cv p = inRange (bounds cv) p && False == cv!p

digLeft (i,j,d,cv) = isClear cv (i,j-1) >> isNotClear cv (i+1,j-1) >> return (i,j,d+1,cv//[((i+1,j-1),False)])
digRight (i,j,d,cv) = isClear cv (i,j+1) >> isNotClear cv (i+1,j+1) >> return (i,j,d+1,cv//[((i+1,j+1),False)])

fall (-1) _ _ = Nothing
fall f cv (i,j,d) = if isClear' cv (i+1,j)
                    then fall (f-1) cv (i+1,j,d)
                    else return (i,j,d,cv)

makeNode s@(i,_,d,cv) = (d+maxDig, s)
    where
      rf = [ length . filter id $ [ cv!(r,j) | j <- [1..cols] ]
             | r <- [i+1..rows] ]
      rf' = groupBy streak rf
      streak a b = a==cols && b==cols
      streakWeight s = (s * (s+1)) `div` 2
      (rows, cols) = snd (bounds cv)
      maxDig = sum $ map (streakWeight . length) rf'

height cv = fst . snd $ bounds cv

show' Nothing = "No"
show' (Just j) = "Yes " ++ show j

solveDP :: Int -> Cave -> Maybe Int
solveDP f cv = s (1,1) [] []
    where
      s :: (Int,Int) -> [Int] -> [Int] -> Maybe Int
      s = memo3 (pair integral integral) (list integral) (list integral) (solveDP' s)
      solveDP' :: ((Int,Int) -> [Int] -> [Int] -> Maybe Int) ->
                  (Int,Int) -> [Int] -> [Int] -> Maybe Int
      solveDP' _ (i,_) _ _ | i == (fst . snd $ bounds cv) = Just 0
      solveDP' s (i,j) h1 h2 = if not $ null directs
                               then Just $ minimum directs
                               else if not $ null indirects
                                    then Just $ minimum indirects
                                    else Nothing
          where directs = catMaybes $ map fall' range
                range = [j-left..j+right]
                left = length $ takeWhile (not . (clear (i+1) h2) . succ) $ 
                       takeWhile (clear i h1) $ map (j-) [1..]
                right = length $ takeWhile (not . (clear (i+1) h2) . pred) $
                        takeWhile (clear i h1) $ map (j+) [1..]
                clear i h j = j `elem` h || isClear' cv (i,j)
                fall' j = do
                  guard $ clear (i+1) h2 j
                  (i',j',_,_) <- fall (f-1) cv (i+1,j,undefined)
                  guard $ i' > i
                  let h' = if i' == i+1 then h2 else []
                  s (i',j') h' []
                indirects = map succ . catMaybes $ concatMap dig [left'..right']
                left' = head $ filter (not . clear (i+1) h2) range
                right' = last $ filter (not . clear (i+1) h2) range
                dig j' = [ guard (j' < right') >> s (i, max j (j'+1)) h1 h',
                           guard (j' > left') >> s (i, min j (j'-1)) h1 h']
                    where h' = insert j' h2

min' Nothing b = b
min' a Nothing = a
min' (Just a) (Just b) = Just $ min a b

minimum' = foldl' min' Nothing

solveDP2 f0 cv = fall f0 1 1 1 1
    where 
      fall (-1) _ _ _ _ = Nothing
      fall f i j h1 h2 | h2 < h1 = fall f i j h2 h1
      fall f i j h1 h2 = if full i' j
                         then minimum' [
                                s i jmin (1) l,
                                s i jmax (-1) l,
                                walkLeft,
                                walkRight
                              ]
                         else fall (f-1) i' j j j
          where jmin = last $ takeWhile walkable $ map (j-) [0..j-1]
                jmin' = pred jmin
                jmax = last $ takeWhile walkable $ map (j+) [0..cols-j]
                jmax' = succ jmax
                walkable = clear &&& full i' >>> uncurry (&&)
                i' = succ i
                l = jmax - jmin
                full i j = i > rows || cv!(i,j)
                clear j = (h1 <= j && j <= h2) || not (full i j)
                walkLeft = guard (jmin'>=1 && clear jmin') >>
                           fall (f0-1) i' jmin' jmin' jmin'
                walkRight = guard (jmax' <= cols && clear jmax') >>
                            fall (f0-1) i' jmax' jmax' jmax'
      (rows,cols) = snd $ bounds cv
      s = memo4 (arrayRange (1,50)) (arrayRange (1,50)) dir (arrayRange (0,49)) s'
      s' i _ _   _ | i == rows = Just 0
      s' i j dir l = minimum' $
            [ fall (f0-1) i' (j+dir*(c-1)) (j+dir*(c-d)) (j+dir*(c-1)) >>=
              return . (+d)
              | c <- [1..l], d <- [1..c] ]
          where i' = succ i

memo4 :: Memo a -> Memo b -> Memo c -> Memo d -> (a -> b -> c -> d -> r) -> a -> b -> c -> d -> r
memo4 a b c d = a . (memo3 b c d .)

boolToDir True = 1
boolToDir False = -1
dirToBool = (>0)
dir = wrap boolToDir dirToBool bool

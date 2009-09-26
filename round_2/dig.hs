import qualified Data.Set as S
import qualified Data.Heap as H
import Data.Array
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    [r,c,f] <- map read . words <$> getLine
    cv <- cave r c <$> replicateM r getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show' (solve f cv)

cave r c xs = listArray ((1,1),(r,c)) $ concat xs

r (r,_,_,_) = r

type Cave = Array (Int,Int) Char
type State = (Int,Int,Int,Cave)
type Node = (Int,State)

solve f cv = astar goal (nextStates f) (H.singleton (0,(1,1,0,cv))) S.empty

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

nextStates f s@(i,j,d,cv) = catMaybes [
                             goLeft f s,
                             goRight f s,
                             digLeft s,
                             digRight s
                            ]

goLeft f (i,j,d,cv) = isClear cv (i,j-1) >> fall f cv (i,j-1,d)
goRight f (i,j,d,cv) = isClear cv (i,j+1) >> fall f cv (i,j+1,d)

isClear cv = guard . isClear' cv
isNotClear cv = guard . not . isClear' cv
isClear' cv p = inRange (bounds cv) p && '.' == cv!p

digLeft (i,j,d,cv) = isClear cv (i,j-1) >> isNotClear cv (i+1,j-1) >> makeNode (i,j,d+1,cv//[((i+1,j-1),'.')])
digRight (i,j,d,cv) = isClear cv (i,j+1) >> isNotClear cv (i+1,j+1) >> makeNode (i,j,d+1,cv//[((i+1,j+1),'.')])

fall (-1) _ _ = Nothing
fall f cv (i,j,d) = if isClear' cv (i+1,j)
                    then fall (f-1) cv (i+1,j,d)
                    else makeNode (i,j,d,cv)

makeNode s@(i,_,d,cv) = return (d+maxDig, s)
    where rf = length . filter ((==cols) . length ) . group . map (fst . fst ) . filter ((=='#') . snd) . filter ((>i) . fst . fst) $ assocs cv
          (rows, cols) = snd (bounds cv)
          maxDig = (rf * (rf+1)) `div` 2

height cv = fst . snd $ bounds cv

show' Nothing = "No"
show' (Just j) = "Yes " ++ show j

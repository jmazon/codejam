import Data.Array
import Data.Functor
import Data.Maybe
import Data.List
import Data.Function
import Control.Monad

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    (k,d) <- readDiamond
    let x = minimumBy (compare `on` abs) (horizSymetries k d)
        y = minimumBy (compare `on` abs) (verticSymetries k d)
    putStrLn $ "Case #" ++ show t ++ ": " ++ show (increase k (abs x + abs y))

readDiamond = do
  k <- read <$> getLine
  ds <- concat <$> replicateM (2*k-1) (map read . words <$> getLine)
  let starts = [0,-1..(-k+1)] ++ [-k+2,-k+3..0] :: [Int]
      rowIndices = map (\s -> [s,s+2..(-s)]) starts :: [[Int]]
      indices = concat $ ann [-k+1..k-1] rowIndices :: [(Int,Int)]
      elems = zip indices (Just <$> ds) :: [((Int,Int),Maybe Int)]
  let d = accumArray (flip const) Nothing ((-k+1,-k+1),(k-1,k-1)) elems
  return (k,d)

ann xs yss = zipWith f xs yss
    where f x ys = zipWith (,) (repeat x) ys

isHorizSymetricalOn k d x = and [ isOut d (i,x-j) || isOut d (i,x+j) ||
                                  d!(i,x-j) == d!(i,x+j) 
                                      | i <- [-k+1..k-1],
                                        j <- [1..k-1] ]

isVerticSymetricalOn k d y = and [ isOut d (y-i,j) || isOut d (y+i,j) ||
                                   d!(y-i,j) == d!(y+i,j) 
                                       | i <- [1..k-1],
                                         j <- [-k+1..k-1] ]

horizSymetries k d = filter (isHorizSymetricalOn k d) [-k+1..k-1]
verticSymetries k d = filter (isVerticSymetricalOn k d) [-k+1..k-1]

isOut d p = not (inRange (bounds d) p) || isNothing (d!p)

increase k 0 = 0
increase k n = 2*k+1 + increase (k+1) (n-1)

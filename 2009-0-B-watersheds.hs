import Data.List
import Data.Array
import Data.Function
import Control.Monad
import qualified Data.Map as M

type Coord = (Int, Int)
type Alt = Int
type Map = Array Coord

main = do
  tests <- read `liftM` getLine
  forM_ [1..tests] $ \i -> do
         putStrLn $ "Case #" ++ (show i) ++ ":"
         runTest

runTest = do
  [h, w] <- (map read . words) `liftM` getLine
  altMap <- readMap h w `liftM` replicateM h getLine
  let flowMap = array (bounds altMap) $
                [ (c, flow altMap c) | c <- indices altMap ]
  let sinkMap = let s = sink sinkMap flowMap
                in array (bounds flowMap) $
                   [ (c, s c) | c <- indices flowMap ]
  let alphaMap = tail $ map fst $
                 scanl alphatize (undefined, ('a', M.empty)) $
                 elems sinkMap
  printMap h w alphaMap

readMap :: Int -> Int -> [String] -> Map Alt
readMap h w ls = listArray ((1,1), (h,w)) $
                 map read $
                 concatMap words ls

flow :: Map Alt -> Coord -> Coord
flow m c = minimumBy (compare `on` (m!)) (neighbors (bounds m) c)

neighbors :: (Coord,Coord) -> Coord -> [Coord]
neighbors b (y,x) = filter (inRange b)
                    [(y,x), (y-1,x), (y,x-1), (y,x+1), (y+1,x)]

sink :: Map Coord -> Map Coord -> Coord -> Coord
sink s f c | c' == c   = c
           | otherwise = s!c'
    where c' = f!c

alphatize (_, s@(i, a)) c = case M.lookup c a of
                              Just l -> (l, s)
                              Nothing -> (i, (succ i, M.insert c i a))
printMap 0 _ _ = return ()
printMap h w m = do
  putStrLn $ intersperse ' ' l
  printMap (h-1) w m'
    where (l, m') = splitAt w m
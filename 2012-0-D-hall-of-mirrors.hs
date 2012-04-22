import Control.Monad (forM_,replicateM,guard)
import Data.Maybe (fromJust)
import Data.Array
import Data.List
import Data.Ratio

import Debug.Trace
tr x = traceShow x x

type Rat = Ratio Int

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [h,w,d] <- fmap (map read . words) getLine
    raw <- fmap concat (replicateM h getLine)
    let (y,x) = fromJust (elemIndex 'X' raw) `divMod` w
        r = listArray ((0,0),(h-1,w-1)) (map (== '#') raw)
    return ()

fractions n = do
  j <- [1..n]
  i <- [1..j-1]
  guard (gcd i j == 1)
  return (Dir (fromIntegral i) (fromIntegral j))
omnidir n = map (uncurry Dir) [ (0,1), (1,1), (1,0), (1,-1)
                              , (0,-1), (-1,-1), (-1,0), (-1,1) ] ++
            fs ++
            map flipH fs ++
            map flipV fs ++
            map invert fs
    where fs = fractions n
normalize (Dir y x) = Dir (y / m) (x / m)
    where m = max y x

flipH (Dir i j) = Dir i (-j)
flipV (Dir i j) = Dir (-i) j
invert (Dir i j) = Dir (-i) (-j)


solve y x r d = start
    where start = go (State y (x+1) 0 (-1%2) (Dir 0 1))
          go s | traceShow s False = undefined
               | not (r!(y,x))     = traceShow "Free" $ go (free s)
               | isAbsorbed s r    = traceShow "Absorbed" Nothing
               | otherwise         = traceShow "Bounce" $ go (bounce s)
               where y = gridY s
                     x = gridX s

isAbsorbed (State y x ry rx (Dir dy dx)) r
    = abs rx == 1%2 &&
      abs ry == 1%2 &&
      not (r!(y - numerator (signum dy),x)) &&
      not (r!(y,x - numerator (signum dx)))

bounce (State y x ry rx d)
    = State (y + if by then numerator (signum ry) else 0)
            (x + if bx then numerator (signum rx) else 0)
            (if by then -ry else ry)
            (if bx then -rx else rx)
            ( (if by then flipV else id) $
              (if bx then flipH else id) d)
    where by = abs ry == 1%2
          bx = abs rx == 1%2

free s = State (gridY s + if up then -1 else if down then 1 else 0)
               (gridX s + if left then -1 else if right then 1 else 0)
               (normRel relY')
               (normRel relX')
               (dir s)
    where relY' = relY s + dirY (dir s)
          relX' = relX s + dirX (dir s)
          left = relX' <= -1%2
          right = relX' >= 1%2
          up = relY' <= -1%2
          down = relY' >= 1%2

normRel f | f < -1%2  = f + 1
          | f >  1%2  = f - 1
          | otherwise = f

data Dir = Dir { dirY :: !Rat, dirX :: !Rat } deriving Show

data State = State {
      gridY :: !Int
    , gridX :: !Int
    , relY :: !Rat
    , relX :: !Rat
    , dir :: !Dir
    } deriving Show


next s (dy,dx) = undefined

uncurry4 f (a,b,c,d) = f a b c d

case1,case2,case3,case4,case5,case6 :: (Int,Int,Array (Int,Int) Bool,Int)
case1 = (1,1,array ((0,0),(2,2)) [((0,0),True),((0,1),True),((0,2),True),((1,0),True),((1,1),False),((1,2),True),((2,0),True),((2,1),True),((2,2),True)],1)
case2 = (1,1,array ((0,0),(2,2)) [((0,0),True),((0,1),True),((0,2),True),((1,0),True),((1,1),False),((1,2),True),((2,0),True),((2,1),True),((2,2),True)],2)
case3 = (1,1,array ((0,0),(3,2)) [((0,0),True),((0,1),True),((0,2),True),((1,0),True),((1,1),False),((1,2),True),((2,0),True),((2,1),False),((2,2),True),((3,0),True),((3,1),True),((3,2),True)],8)
case4 = (3,3,array ((0,0),(6,6)) [((0,0),True),((0,1),True),((0,2),True),((0,3),True),((0,4),True),((0,5),True),((0,6),True),((1,0),True),((1,1),False),((1,2),False),((1,3),False),((1,4),False),((1,5),False),((1,6),True),((2,0),True),((2,1),False),((2,2),False),((2,3),False),((2,4),False),((2,5),False),((2,6),True),((3,0),True),((3,1),False),((3,2),False),((3,3),False),((3,4),False),((3,5),False),((3,6),True),((4,0),True),((4,1),False),((4,2),False),((4,3),False),((4,4),False),((4,5),True),((4,6),True),((5,0),True),((5,1),False),((5,2),False),((5,3),False),((5,4),False),((5,5),False),((5,6),True),((6,0),True),((6,1),True),((6,2),True),((6,3),True),((6,4),True),((6,5),True),((6,6),True)],4)
case5 = (1,3,array ((0,0),(4,5)) [((0,0),True),((0,1),True),((0,2),True),((0,3),True),((0,4),True),((0,5),True),((1,0),True),((1,1),False),((1,2),False),((1,3),False),((1,4),False),((1,5),True),((2,0),True),((2,1),False),((2,2),True),((2,3),False),((2,4),False),((2,5),True),((3,0),True),((3,1),False),((3,2),False),((3,3),False),((3,4),True),((3,5),True),((4,0),True),((4,1),True),((4,2),True),((4,3),True),((4,4),True),((4,5),True)],3)
case6 = (1,3,array ((0,0),(4,5)) [((0,0),True),((0,1),True),((0,2),True),((0,3),True),((0,4),True),((0,5),True),((1,0),True),((1,1),False),((1,2),False),((1,3),False),((1,4),False),((1,5),True),((2,0),True),((2,1),False),((2,2),True),((2,3),False),((2,4),False),((2,5),True),((3,0),True),((3,1),False),((3,2),False),((3,3),False),((3,4),True),((3,5),True),((4,0),True),((4,1),True),((4,2),True),((4,3),True),((4,4),True),((4,5),True)],10)

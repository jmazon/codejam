import Data.Array
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Arrow
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    b <- readBoard
    let s = solve b
    putStrLn $ "Case #" ++ show t ++ ": " ++ show (length s)
    forM_ s $ \(s,c) -> putStrLn $ (show s) ++ " " ++ (show c)

readBoard = do
  [m,n] <- map read . words <$> getLine
  listArray ((0,0),(m-1,n-1)) . concat <$>
    replicateM m (concatMap decodeChar <$> getLine)

decodeChar c = reverse [testBit n i | i <- [0..3] ]
    where n = digitToInt c

-- find out cutout candidates
candidates b = r where
  extend ((i,j),c) = if null xs then 1 else minimum xs
      where xs = (map possibly [ (i-1,j-1,c)
                               , (i-1,j,not c)
                               , (i,j-1,not c)])
  possibly (i,j,c) = if inRange (bounds b) (i,j) && b!(i,j) == c
                     then 1+r!(i,j)
                     else 1
  r = listArray (bounds b) $ map extend $ assocs b

bestCutout c = if isNothing (snd r) then Nothing else Just $ (second fromJust) r
    where f s@(v,_) (i,v') = if v' > v then (v',Just i) else s
          r = foldl' f (0,Nothing) $ assocs c

cutout c (s,(i0,j0)) = c' where
  c' = c // changes
  (m,n) = snd $ bounds c
  changes = map f [ (i,j) | i <- [i0-s+1..min m (i0+s)]
                          , j <- [j0-s+1..min n (j0+s)] ]
  f (i,j) | i <= i0 && j <= j0 = ((i,j),0)
          | otherwise          = ((i,j),min (c!(i,j)) (max (i-i0) (j-j0)))

solve b = map (head&&&length) $ group $ fst <$> solve' (candidates b)

solve' c | isJust b  = b' : solve' (cutout c b')
         | otherwise = []
    where b = bestCutout c
          b' = fromJust b

-- debug
printBoard b = do
  let (m,n) = snd (bounds b)
  forM_ [0..m] $ \i -> do
    forM_ [0..n] $ \j -> putChar $ if b!(i,j) then '1' else '0'
    putChar '\n'


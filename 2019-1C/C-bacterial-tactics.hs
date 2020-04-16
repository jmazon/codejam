{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Bits
import Data.List
import Data.Array
import Control.Monad

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [r,c] <- map read . words <$> getLine
    g <- (map . map) (== '#') <$> replicateM r getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve (Row r) (Col c) g)

newtype Row = Row { getRow :: Int } deriving (Eq,Ord,Ix,Enum,Num)
newtype Col = Col { getCol :: Int } deriving (Eq,Ord,Ix,Enum,Num)
newtype Nimber = Nimber Int deriving (Eq,Enum,Num,Bits)
type St = (Row,Col,Row,Col)
data Split = RowSplit Row | ColSplit Col

solve :: Row -> Col -> [[Bool]] -> Int
solve r c g = result
  where
    nimber :: St -> Nimber
    nimber s@(i1,j1,i2,j2) | i2 < i1 || j2 < j1 = 0
                           | inRange (bounds states) s = states ! s
  
    states = array ((1,1,1,1),(r,c,r,c))
             [ ((i1,j1,i2,j2),(compNimber <*> spawns)(i1,j1,i2,j2))
             | i1 <- [1..r], j1 <- [1..c] , i2 <- [i1..r], j2 <- [j1..c] ]
    compNimber :: St -> [Split] -> Nimber
    compNimber s = mex . map (rec . split s)
      where rec (st1,st2) = nimber st1 `xor` nimber st2
    mex :: [Nimber] -> Nimber
    mex gs = head $ filter (`notElem` gs) [0..]
  
    split :: St -> Split -> (St,St)
    split (i1,j1,i2,j2) s = case s of
      RowSplit i -> ( (i1,j1,i-1,j2), (i+1,j1,i2,j2) )
      ColSplit j -> ( (i1,j1,i2,j-1), (i1,j+1,i2,j2) )
  
    spawns :: St -> [Split]
    spawns (i1,j1,i2,j2) =
      map RowSplit [ i | i <- [i1..i2]
                       , all (< j1) [ rowHelper!(i,j) | j <- [j1..j2] ] ] ++
      map ColSplit [ j | j <- [j1..j2]
                       , all (< i1) [ colHelper!(i,j) | i <- [i1..i2] ] ]
      where rowHelper :: Array (Row,Col) Col
            rowHelper = rcHelper id id g
            colHelper :: Array (Row,Col) Row
            colHelper = rcHelper transpose transpose g
            rcHelper g h i = listArray ((1,1),(r,c)) $ concat $ g $
                             map (snd . mapAccumL f 0 . zip [1..]) $ h i
              where f a (j,True)  = (j,j)
                    f a (_,False) = (a,a)
  
    mult :: Split -> Int
    mult (RowSplit _) = getCol c
    mult (ColSplit _) = getRow r
  
    s0 = (1,1,r,c)

    winnable :: Split -> Bool
    winnable sp = compNimber s0 [sp] /= 0
  
    result = sum $ map mult $ filter winnable $ spawns s0

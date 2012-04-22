import Data.Array
import Data.Functor
import Data.List
import Control.Applicative
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    petri <- readBacteria
    let Just r = findIndex isDead (iterate step petri)
    putStrLn $ "Case #" ++ show t ++ ": " ++ show r

readBacteria = do
  t <- read <$> getLine
  bs <- replicateM t (map read . words <$> getLine) :: IO [[Int]]
  let (ZipList [_,_,c,r]) = foldl1' (liftA2 max) (ZipList <$> bs)
  return $ accumArray (flip const) False ((0,0),(r,c)) (concat $ bacteriaBlock <$> bs)

bacteriaBlock [x1,y1,x2,y2] = [ ((y,x),True) | y <- [y1..y2], x <- [x1..x2] ]

step b = array (bounds b) $ map f (indices b)
    where f p@(0,_) = (p,False)
          f p@(_,0) = (p,False)
          f p@(i,j) = if b!(i,j)
                      then (p,b!(i-1,j) || b!(i,j-1))
                      else (p,b!(i-1,j) && b!(i,j-1))

isDead :: Array (Int,Int) Bool -> Bool
isDead = not . or . elems
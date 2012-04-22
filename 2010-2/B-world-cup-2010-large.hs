import Data.List
import Data.Functor
import Control.Monad
import System.IO

data Node = Node { maxMisses :: Int, priceByMiss :: [Int] }

team m = Node m [0]

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    p <- read <$> getLine
    ts <- map (team . read) . words <$> getLine
    ps <- replicateM p (map read . words <$> getLine)
    let tr = foldl' newRound ts ps
    putStrLn $ "Case #" ++ show t ++ ": " ++ show 0

pairs (a:b:xs) = (a,b) : pairs xs
pairs [] = []

newRound :: [Node] -> [Int] -> [Node]
newRound ts ps = zipWith match (pairs ts) ps

match (t1,t2) p = 
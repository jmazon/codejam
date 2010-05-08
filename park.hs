import Control.Monad.Reader
import Data.Array
import Data.Functor

readIntLine = fmap read . words <$> getLine

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \tt -> do
    [r, k, n] <- readIntLine
    gs <- listArray (0,n-1) <$> readIntLine
    putStrLn $ "Case #" ++ show tt ++ ": "

gs :: Groups
gs = listArray (0,3) [1,4,2,1]

type Group = Int
type GroupSize = Int
type Groups = Array Group GroupSize
data Env = Env { capacity :: GroupSize
               , groups :: Groups }

boardCoaster e (i,r) = boardCoaster' e (i,r) i (capacity e)
boardCoaster' e (i,r) i0 c =
    let s = nextGroupSize e i
        i' = nextGroup e i
    in if c >= s && (i /= i0 || c == capacity e)
       then boardCoaster' e (i',r + fromIntegral s) i0 (c-s)
       else (i,r) -- coaster full

nextGroupSize e i = groups e ! i
nextGroup e i = mod (succ i) (capacity e)
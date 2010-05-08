import Control.Monad.Reader
import Data.Array
import Data.Functor

readIntLine = fmap read . words <$> getLine

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \tt -> do
    [r, k, n] <- readIntLine
    gs <- listArray (0,n-1) <$> readIntLine
    let park = Park r k n gs
    putStrLn $ "Case #" ++ show tt ++ ": "
               ++ show (dayIncome park)

type Group = Int     -- actually an index
type GroupSize = Int -- enough in both small and large sets
type GroupList = Array Group GroupSize
data Park = Park { runs :: Int
                 , capacity :: GroupSize
                 , groupCount :: Group
                 , groups :: GroupList }

boardCoaster e (i,r) = boardCoaster' e (i,r) i (capacity e)
boardCoaster' e (i,r) i0 c =
    let s = nextGroupSize e i
        i' = nextGroup e i
    in if c >= s &&                     -- coaster full
          (i /= i0 || c == capacity e)  -- queue exhausted
       then boardCoaster' e (i',r + fromIntegral s) i0 (c-s)
       else (i,r)

nextGroupSize e i = groups e ! i
nextGroup e i = mod (succ i) (groupCount e)

dayIncome e = snd $ iterate (boardCoaster e) (0,0) !! runs e

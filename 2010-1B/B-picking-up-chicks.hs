import Data.List
import Control.Applicative
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  c <- read <$> getLine
  forM_ [1..c] $ \i -> do
    [n,k,b,t] <- map read . words <$> getLine :: IO [Integer]
    xs <- ZipList . map read . words <$> getLine
    vs <- ZipList . map read . words <$> getLine
    let (ZipList cs) = (,) <$> xs <*> vs
        wf = reverse $ map (wouldFinish b t) cs
    putStrLn $ "Case #" ++ show i ++ ": " ++ show' (countSwaps wf k 0)

wouldFinish b t (x,v) = x + v*t >= b

countSwaps _ 0 _          = Just 0
countSwaps [] _ _         = Nothing
countSwaps (True:cs) k n  = (+n) <$> countSwaps cs (k-1) n
countSwaps (False:cs) k n = countSwaps cs k (n+1)

show' (Just n) = show n
show' Nothing  = "IMPOSSIBLE"

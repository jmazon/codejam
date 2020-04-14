import Data.Char
import Data.List
import System.IO
import Control.Monad

main = do
  hSetBuffering stdout LineBuffering
  t <- readLn
  replicateM_ t $ do
    n <- read . head . words <$> getLine
    let stripe w = replicate w '0' ++ replicate w '1'
    mapM_ (putStrLn . take n . cycle . stripe) [8,4,2,1]
    putStrLn . unwords . map show . diffIndices [0..n-1] (cycle [0..15]) .
      map (foldl1 (\a b -> 2*a + b) . map digitToInt) . transpose =<<
        replicateM 4 getLine
    "1" <- getLine; return ()

diffIndices (i:is) (a:as) bs@(b:bs') | a == b    =     diffIndices is as bs'
                                     | otherwise = i : diffIndices is as bs
diffIndices is _ [] = is

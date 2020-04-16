import Data.Ord
import Data.Either
import Data.List (sort,maximumBy)
import Control.Monad (forM_,replicateM)
import Control.Arrow ((***))

import Data.Monoid ((<>)) -- FSCKin GCJ obsolete GHC

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [p,q] <- map read . words <$> getLine
    people <- replicateM p (words <$> getLine)
    let dirs = partitionEithers $ sort $ flip map people $ \[xi,yi,d] -> do
          case d of "N" -> Left (read yi+1,1)
                    "S" -> Left (read yi,-1)
                    "E" -> Right (read xi+1,1)
                    "W" -> Right (read xi,-1)
        f = fst . maximumBy (comparing snd <> comparing (Down . fst)) .
            uncurry zip . ( (0 :) *** (scanl (+) 0)) . unzip
        (y,x) = (f *** f) dirs
    putStrLn $ "Case #" ++ show i ++ ": " ++ show x ++ " " ++ show y

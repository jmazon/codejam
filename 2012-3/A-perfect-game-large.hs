import Data.List (sort)
import Control.Monad (forM_,liftM,liftM2)
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    getLine -- n <- readLn
    xs <- liftM2 (zipWith (/)) (liftM (map read . words) getLine)
                               (liftM (map read . words) getLine)
    putStrLn $ "Case #" ++ show i ++ ": " ++
               unwords (map (show . snd) $ sort $ zip xs [0..])


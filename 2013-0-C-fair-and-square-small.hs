import Data.Maybe (catMaybes)
import Control.Monad (forM_)

isFair n = show n == reverse (show n)
isSquare n | n == r^2  = Just r
           | otherwise = Nothing
    where r = round (sqrt (fromIntegral n))

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [a,b] <- fmap (map read . words) getLine
    let n = length $
            filter isFair $
            catMaybes $ map isSquare $
            filter isFair [a..b]
    putStrLn $ "Case #" ++ show i ++ ": " ++ show n

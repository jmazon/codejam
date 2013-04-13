import Control.Monad (forM_)

isFair n = show n == reverse (show n)
square n = n * n
isFairAndSquareR n = isFair n && isFair (square n)

fas = map square $ filter isFairAndSquareR [1..10^7]

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [a,b] <- fmap (map read . words) getLine
    let n = length $ filter (>= a) $ filter (<= b) fas
    putStrLn $ "Case #" ++ show i ++ ": " ++ show n

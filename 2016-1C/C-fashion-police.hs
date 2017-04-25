import Control.Monad (forM_)
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [j,p,s,k] <- map read . words <$> getLine
    let s' = min k s
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (j * p * s')
    mapM_ (putStrLn . unwords . map (show . succ))
      [ [j,p,(j+p+d) `mod` s] | j <- [0..j-1], p <- [0..p-1], d <- [0..s'-1] ]

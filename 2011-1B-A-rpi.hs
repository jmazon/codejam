import Control.Monad (forM_,replicateM)
import Data.Array
import Data.Maybe

main = do
  t <- readLn
  forM_ [1..t] $ \c -> do
    n <- readLn
    s <- fmap (listArray ((1,1),(n,n)) . map readS . concat)
              (replicateM n getLine)
    let owp = [ average [ calcWp $ matches s n i j 
                        | j <- [1..n], isJust (s!(i,j)) ]
              | i <- [1..n] ]
    putStrLn $ "Case #" ++ show c ++ ":"
    forM_ [1..n] $ \i -> do
      let wp = calcWp (matches s n 0 i)
      let oowp = average $ catMaybes $
                 zipWith (fmap . const) owp
                 [ s!(i,j) | j <- [1..n] ]
      print $ 0.25 * wp + 0.50 * (owp!!(i-1)) + 0.25 * oowp

readS '.' = Nothing
readS '1' = Just True
readS '0' = Just False

matches s n e i = catMaybes [ s!(i,j) | j <- [1..n], j /= e ]

calcWp rs = fromIntegral (length (filter id rs)) / fromIntegral (length rs)

average xs = sum xs / fromIntegral (length xs)
import Data.Array
import Data.Maybe
import Control.Monad

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    m <- liftM (listArray ((1,1),(n,n)) . map toVictory . concat)
         (replicateM n getLine)
    let owp = listArray (1,n) 
              [ avg [ wp n m o t | o <- [1..n], isJust (m!(t,o)) ]
                | t <- [1..n] ]
        wps = map (flip (wp n m) 0) [1..n]
        owps = elems owp
        oowps = [ avg [ owp!o | o <- [1..n], isJust (m!(t,o)) ] | t <- [1..n] ]
        rpis = zipWith3 (\w ow oow -> 0.25*w + 0.5*ow + 0.25*oow) wps owps oowps
    putStrLn $ "Case #" ++ show i ++ ": "
    mapM_ print rpis

toVictory '.' = Nothing
toVictory '0' = Just False
toVictory '1' = Just True

wp n m t x = fromIntegral (length (filter id gs)) / fromIntegral (length gs)
    where gs = catMaybes [ m!(t,o) | o <- [1..n], o /= x ]

avg l = sum l / fromIntegral (length l)

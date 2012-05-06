import Control.Monad
import Data.List
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    --[n,m] <- fmap (map read . words) getLine
    getLine
    ns <- fmap (pairs . map read . words) getLine
    ms <- fmap (pairs . map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (lcs ns ms)
pairs (x:y:zs) = (x,y) : pairs zs
pairs _ = []
lcs ((0,_):ns) ms = lcs ns ms
lcs ns ((0,_):ms) = lcs ns ms
lcs ((a,aa):ns) ((b,bb):ms)
    | aa == bb = let c = min a b
                 in c + lcs ((a-c,aa):ns) ((b-c,bb):ms)
    | otherwise = max (lcs ((a,aa):ns) (dropWhile ((/= aa) . snd) ms))
                      (lcs ns ((b,bb):ms))
lcs _ _ = 0

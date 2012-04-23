import Data.Maybe
import Data.List
import Control.Monad (forM_,liftM)

main = do
  n <- readLn
  forM_ [1..n] $ \i -> do
    m <- readLn
    ps <- (map read . words) `liftM` getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ case solve m ps of
      Nothing -> "IMPOSSIBLE"
      Just (a,b,p) -> show a ++ " " ++ show b ++ " " ++ show p

solve m ps = if p > 0 then Just (a,b,p) else Nothing
    where (p,bp,sp) = maximum $ map (solve' m) $ tails ps
          a = 1 + fromJust (elemIndex (-bp) ps)
          b = 1 + a + fromJust (elemIndex sp (drop a ps))
solve' m (p:ps@(_:_)) | p <= m = ( (p'-p) * (m `div` p), -p, p' )
  where p' = maximum ps
solve' _ _ = (0,0,-1)
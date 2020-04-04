import Control.Monad
import Data.List
import Data.Maybe
readPair l = (a,b) where [a,b] = map read (words l)
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    as <- replicateM n (readPair <$> getLine)
    let rs = fmap (map snd . sort) $ assign (0,0) (sortOn snd (zip [0..] as))
    putStrLn $ "Case #" ++ show i ++ ": " ++ fromMaybe "IMPOSSIBLE" rs
assign (c,j) ((i,(s,e)):t)
  | c <= s = fmap ((i,'C') :) (assign (e,j) t)
  | j <= s = fmap ((i,'J') :) (assign (c,e) t)
  | otherwise = Nothing
assign _ [] = Just []

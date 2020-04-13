import Control.Monad
import Data.List
import Data.List.Split
main = do
  t <- readLn
  forM [1..t] $ \i -> do
    n <- readLn
    mbFold <- foldM expand mempty <$> replicateM n getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ maybe "*" writeOut mbFold
writeOut (pref,inf,suf) = pref ++ inf ++ suf
expand (pre,mid,suf) l = do
  let pre':rest = splitOn "*" l
  pre'' <- commonOp isPrefixOf pre pre'
  suf'' <- commonOp isSuffixOf suf (last rest)
  pure (pre'',concat (init rest) ++ mid,suf'')
commonOp op a b | a `op` b  = Just b
                | b `op` a  = Just a
                | otherwise = Nothing

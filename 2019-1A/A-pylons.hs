{-# LANGUAGE LambdaCase #-}
import System.Random
import Control.Monad.State
import qualified Data.Set as S

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \tn -> do
    [r,c] <- map read . words <$> getLine
    putStr $ "Case #" ++ show tn ++ ": "
    g <- getStdGen
    case solve g r c of
      Nothing -> putStrLn "IMPOSSIBLE"
      Just ps -> putStrLn "POSSIBLE" *>
        forM_ ps (\(i,j) -> putStrLn $ show i ++ " " ++ show j)

attempts :: Int
attempts = 10

retry :: Monad m => Int -> m (Maybe a) -> m (Maybe a)
retry 0 _ = return Nothing
retry n m = m >>= maybe (retry (n-1) m) (return . Just)

pick :: S.Set a -> State StdGen (Maybe a)
pick s | S.null s = pure Nothing
       | otherwise = Just . flip S.elemAt s . pred <$>
                       (state . randomR) (1,S.size s)

solve :: StdGen -> Int -> Int -> Maybe [(Int,Int)]
solve g r c = evalState (retry attempts build) g where
  s0 = S.fromList $ (,) <$> [1..r] <*> [1..c]
  build = go (1000,501) s0 []
  go prev s path | S.null s = return (Just path)
                 | otherwise = do
    pick (S.filter (compatible prev) s) >>= \case
      Nothing -> return Nothing
      Just p  -> go p (S.delete p s) (p:path)
  compatible (i,j) (k,l) = i /= k && j /= l && abs (k-i) /= abs (l-j)

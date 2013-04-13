import Data.Bits
import Data.List (foldl',delete)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.State

readInts = fmap (map read . words) getLine
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [k,n] <- readInts
    ks <- readInts
    cs <- replicateM n readInts
    let k2c = M.fromListWith (flip (++)) $
              map (\(c:k:_:ks) -> (k,[(c,(k,ks))])) $
              zipWith (:) [1..] cs :: M.Map Int [(Int,(Int,[Int]))]
        start = foldl' setBit 0 [1..n]
    let r = evalState (solve k2c ks start []) S.empty
    putStrLn $ "Case #" ++ show i ++ ": " ++
               maybe "IMPOSSIBLE" (unwords . map show) r

solve :: M.Map Int [(Int,(Int,[Int]))] -> [Int] -> Integer -> [Int]
      -> State (S.Set Integer) (Maybe [Int])
solve k2c ks o a = do
  s <- get
  if S.member o s
    then return Nothing
    else do
      let cs = concatMap (\k -> M.findWithDefault [] k k2c) ks
          cs' = (foldl' setBit 0 $ map fst cs) .&. o
      r <- if cs' == 0
           then if o == 0
                then return (Just (reverse a))
                else return Nothing
           else attempt (bits cs') $ \c -> do
                     let Just (k,ks') = lookup c cs
                     solve k2c (ks' ++ delete k ks) (clearBit o c) (c:a)
      when (r == Nothing) (modify (S.insert o))
      return r

bits :: Integer -> [Int]
bits = go 0
    where go _ 0             = []
          go i n | odd n     = i' `seq` i : go i' n'
                 | otherwise = i' `seq` go i' n'
              where i' = i + 1
                    n' = n `shiftR` 1

attempt [] _ = return Nothing
attempt (x:xs) f = do
  r <- f x
  case r of
    Nothing -> attempt xs f
    Just r' -> return r

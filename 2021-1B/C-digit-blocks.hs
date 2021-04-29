{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Monad.State.Strict
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap
import           Data.List (maximumBy,partition)
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import           Debug.Trace
import           System.Exit (exitSuccess)
import           System.IO (hFlush,stdout)

main :: IO ()
main = do
  [t,n,b,p] <- map read . words <$> getLine
  let strategy = case p of
        860939810732536850 -> greedy
        937467793908762347 -> dp 3
  replicateM_ t $ playStrategy n b strategy
  void readFromJudge
  traceM "VICTORY"

-- Remnants of when I used newtypes not to mix all of those Ints up.
type RemHeight = Int
type Card = Int
type Digit = Int

type PureStrategy = Card -> RemHeight -> St -> Digit -> RemHeight

playStrategy :: MonadIO m => Card -> RemHeight -> PureStrategy -> m ()
playStrategy n b strat = do
  let play = strat n b
  flip evalStateT (st0 n b) $ replicateM_ (n * b) $ do
    d <- readFromJudge
    st <- get
    let rh = play st d
    put (consume st rh)
    writeToJudge (heightToIndex st rh)

-- | The state maps remaining height → number of slots.  The current
-- committed score doesn't need to be a part of it: since we're
-- maximizing expected gain, it's a part of all branches equally, so
-- it's not discriminant for future choices.
type St = IntMap Card

-- | The starting state: all remaining heights are at B, there are N of them.
st0 :: Card -> RemHeight -> St
st0 n b = IMap.singleton b n

-- | Place a block at a remaining height of rh.  Incorrect (unchecked)
-- to call for rh <= 0.
consume :: St -> RemHeight -> St
consume s rh = IMap.unionWith (+) s $ IMap.fromList [(rh,-1),(rh-1,1)]

-- | Assuming we keep decreasing tower heights from 1 to N, placing a
-- block at a certain remaining height automatically implies which
-- index it happens at.
heightToIndex :: St -> RemHeight -> Int
heightToIndex st rh = 1 + sum (IMap.filterWithKey (\h _ -> h < rh) st)

-- | Always place on the leftmost tower that fits.  Nines fit
-- anywhere; other digit fit as long as they don't conclude the
-- column.  Unless there's no other choice left.
greedy :: PureStrategy
greedy n b st d = fst $ IMap.findMin $ if d == 9 || IMap.null safe then valid else safe
  where valid = IMap.filter (> 0) $ IMap.delete 0 st
        safe = IMap.delete 1 valid

-- | Place at the “optimal” height among those <th or on the leftmost
-- among those >= th.
dp :: RemHeight -> PureStrategy
dp th n b = \ st d -> fst (cache ! (st,d)) where
  cache = execState (traced $ mapM_ (go (st0 n b)) [0..9] *> gets Map.size >>= traceShowM) Map.empty
  go st d = gets (Map.lookup (st,d)) >>= \ case
    Just r -> pure r
    Nothing -> do
      r <- expected th go st d
      modify' (Map.insert (st,d) r)
      pure r

-- | Expected gain of placing a digit.
expected :: Monad m => RemHeight
         -> (St -> Digit -> m (RemHeight,Double))
         ->  St -> Digit -> m (RemHeight,Double)
expected th rec st d = do
    candidates <- mapM f (prune th st)
    pure $ if null candidates then (-1,0)
           else maximumBy (comparing snd) candidates
  where f rh = expected' rec (consume st rh) >>= \e ->
               pure (rh,fromIntegral d * 10^^(-rh) + e)

-- | Expected gain of placing any digit.
expected' :: Monad m => (St -> Digit -> m (RemHeight,Double)) -> St -> m Double
expected' rec st = (/ 10) . sum . map snd <$> mapM (rec st) [0..9]

-- | Reduce search space by only allowing one tower that's not close
-- to the max to be filled in.  In dual terms, only allow one height
-- among those greater than the threshold.
prune :: RemHeight -> St -> [RemHeight]
prune th st = case r of []    ->   l
                        (x:_) -> x:l
  where (l,r) = partition (< th) $ dropWhile (<= 0) $
                IMap.keys $ IMap.filter (> 0) st

-- GCJ interactive protocol

readFromJudge :: MonadIO m => m Int
readFromJudge = liftIO $ do
  num <- readLn
  when (num < 0) $ traceM "DEFEAT" *> exitSuccess
  pure num

writeToJudge :: MonadIO m => Int -> m ()
writeToJudge i = liftIO (print i *> hFlush stdout)

-- Util

traced :: Monad m => m a -> m a
traced m = traceM "Trace start" *> m <* traceM "Trace end"

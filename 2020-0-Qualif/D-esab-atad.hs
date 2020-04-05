{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
import Data.Bits (xor)
import Data.Char (intToDigit)
import Data.List hiding (insert)
import Data.List.NonEmpty (NonEmpty((:|)),(<|))
import System.Exit (exitFailure)
import System.IO
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except

-- | Read a line from the judge; exit cleanly if it's a "N" that are
-- all interaction-halting as far as the judge is concerned, whether
-- they're caused my a protocol error or a wrong answer.
checkLine :: IO String
checkLine = getLine >>= \case "N" -> exitFailure
                              s   -> pure s

-- | Read a bit from the database.  This is the raw protocol
-- operation, I'm labelling it as unsafe because we don't really from
-- the caller point of view if a quantum fluctuation is liable to
-- happen.
unsafeReadAt :: MonadIO m => Int -> m Bool
unsafeReadAt i = liftIO $ print i *>
                          checkLine >>= \case "0" -> pure False
                                              "1" -> pure True

-- | Safely read a bit from the database.  If reading would cause a
-- quantum fluctuation, report an error instead.
managedReadAt :: (MonadIO m,MonadState Int m,MonadError () m) => Int -> m Bool
managedReadAt i = get >>= \case 10 -> throwError ()
                                _  -> modify (+1) *> unsafeReadAt i

-- | The Pair type represents a database value and its symmetrical
-- counterpart.
type Pair = PairF Bool
data PairF bool = Pair { pairIdx :: Int, pairValue :: bool } deriving Functor

-- | The PairType type represents a pair's category: Even (counterpart
-- is equal; the pair is unaffected by reversals) or Odd (counterpart
-- is complement; reversals and complements have the same effect on
-- the pair).
data PairType = Even | Odd

-- | In addition to `pairIdx` which is a record accessor, `pairIdxC`
-- returns the pair's symmetrical counterpart index.
pairIdxC :: Int -> Pair -> Int
pairIdxC b p = b + 1 - pairIdx p

-- | In addition to `pairValue` which is a record accessor,
-- `pairValueC` returns the pair's symmetrical counterpart value.
pairValueC :: PairType -> Pair -> Bool
pairValueC Even = pairValue
pairValueC Odd = not . pairValue

-- | Read and classify a pair.
readPair :: (MonadError () m,MonadIO m,MonadState Int m,MonadReader Int m)
         => Int -> m (PairType,Pair)
readPair i = do
  b <- ask
  x <- managedReadAt i
  y <- managedReadAt (b+1-i)
  pure (if x == y then Even else Odd,Pair { pairIdx = i, pairValue = x })

-- | A `Batch` groups together pairs of a same known category.  If we
-- manage to track one of the batch's representatives' value between
-- quantum fluctuations, we're able to deduce all other pairs with no
-- further costly information retrieval!
data Batch offset = Empty | Batch offset (NonEmpty Pair) deriving Functor

-- | Bind a floating batch to a specific boolean offset.  This
-- consumes up to one query.
bind :: (MonadIO m,MonadState Int m,MonadError () m)
     => Batch () -> m (Batch Bool)
bind Empty = pure Empty
bind (Batch () ps@(p :| _)) = do v <- managedReadAt (pairIdx p)
                                 pure (Batch (v `xor` pairValue p) ps)

-- | Loosen a bound batch back to floating when we know the next query
-- will cause a quantum fluctuation.
clear :: Batch Bool -> Batch ()
clear = fmap (const ())

-- | Insert a pair in a batch.
insert :: Pair -> Batch Bool -> Batch Bool
insert p Empty = Batch False (pure p)
insert p (Batch b ps) = Batch b ((fmap (xor b) p) <| ps)

-- | Expand a batch to a list of `(Int,Bool)` pairs.
assocs :: Int -> PairType -> Batch Bool -> [(Int,Bool)]
assocs _ _ Empty = []
assocs n t (Batch b ps) = concatMap f ps where
  f p = [ (pairIdx p,pairValue p `xor` b)
        , (pairIdxC n p,pairValueC t p `xor` b) ]

-- | Turn an unordered list of indexed booleans back into a nice
-- string.
reconstruct :: [(Int,Bool)] -> String
reconstruct = map (intToDigit . fromEnum . snd) . sort

-- | Perform the Code Jam judge I/O and tie the high-level pieces
-- together.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [t,b] <- map read . words <$> getLine
  replicateM_ t $ do
    (evens,odds) <- runReaderT (readBlocks Empty Empty [1..b `div` 2]) b
    putStrLn $ reconstruct $ assocs b Even evens ++ assocs b Odd odds
    "Y" <- checkLine  -- still not legal to end a void'ened
    pure undefined    -- block on a monadic pattern bind :-(

-- | Perform a block of queries, maintaining a knowledge base of pair
-- batches between two quantum fluctuations.
readBlocks :: (MonadIO m,MonadReader Int m)
           => Batch () -> Batch () -> [Int] -> m (Batch Bool,Batch Bool)
readBlocks ftEvens ftOdds indices = do
  (bdEvens',bdOdds',mbIndices') <- flip evalStateT 0 $ do
    Right bdEvens <- runExceptT (bind ftEvens)
    Right bdOdds  <- runExceptT (bind ftOdds)
    readPairs bdEvens bdOdds indices
  case mbIndices' of
    Just indices' -> readBlocks (clear bdEvens') (clear bdOdds') indices'
    Nothing -> pure (bdEvens',bdOdds')

-- | Read, classify and store pairs from the database until the next
-- query would result in a quantum fluctuation.
readPairs :: (MonadIO m,MonadState Int m,MonadReader Int m)
          => Batch Bool -> Batch Bool -> [Int]
          -> m (Batch Bool,Batch Bool,Maybe [Int])
readPairs evens odds [] = pure (evens,odds,Nothing)
readPairs evens odds is@(i:is') = runExceptT (readPair i) >>= \case
  Right (Even,p) -> readPairs (insert p evens) odds is'
  Right (Odd,p)  -> readPairs evens (insert p odds) is'
  Left () -> pure (evens,odds,Just is)

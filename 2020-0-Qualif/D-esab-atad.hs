{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
-- Description: ESAb ATAd
-- Copyright: © 2020 Jean-Baptiste Mazon
-- License: GPL-3
-- Stability: experimental

module Main (
  -- * The problem
  -- $quanta

  -- * The Pair idiom
  -- $pair

  -- ** Pair quality
    PairType(..), pairType

  -- ** Pair data
  , PairF(..), Pair, pairIdxC, pairValueC

  -- * The Batch idiom
  -- $batch
  , Batch(..)

  -- ** Batch subtypes
  , FloatingBatch, BoundBatch, bind, clear

  -- ** Container interface
  , insert, assocs

  -- * Robustness helpers
  -- ** Monad transformers
  -- $robustM
  , managedReadAt, QuantumFluctuation, readPair
  -- $robustM2

  -- ** Newtypes
  -- $robustN
  BitWidth

  -- * Composing a result
  , readBlocks, readPairs
  , reconstruct

  -- * Protocol
  , unsafeReadAt, checkLine
  , main
  ) where

import Data.Bits (xor)
import Data.Char (intToDigit)
import Data.List hiding (insert)
import Data.List.NonEmpty (NonEmpty((:|)),(<|))
import System.Exit (exitFailure)
import System.IO
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Fail

-- $quanta
--
-- That 'QuantumFluctuation' thing is quite the downer.  We can only
-- read 10 bits at the time from the database before everything is
-- shuffled!
--
-- As it turns out, that's not too much of a problem for the easy
-- @B=10@ case: the bits may be shuffled before we even start, but as
-- long as we only query 10 of them, they won't be shuffled again, so
-- our gained knowledge is fresh enough that we can output the result
-- while it's still correct.

-- $pair
--
-- We need to find some way to keep track of the two operations that
-- could happen to a bit in the original database: changes and moves.
-- Changes are what happen when we complement the entire database: the
-- bit's value flips.  Moves are what happen when the database is
-- reversed: the bit's value doesn't change, but its index does.
--
-- So, how are we to make sense of this?  The trick is to always
-- consider a bit as paired with the symmetrical one on the other side
-- of the database, the one it would switch places with if it were
-- reversed.  From a starting database position, we can boil it down
-- to two simple cases:
--
--   * The bits start equal.  In this case, they'll always remain
--     equal, as both operations preserve that.  All we need to track
--     is either of them's value, and we automatically know the other
--     one's without having to query it.  As a parallel to analytical
--     functions, we'll call those 'Even' pairs.
--
--   * The bits start diferent.  In that case, they'll always remain
--     different, as both operations preserve that too.  (It may take
--     just a little more thinking to convince yourself of this, but
--     all there is to it is realizing that both operations have the
--     same start pair and end pair.)  Same as before, all we need to
--     track is either of them's value.  As a parallel to analytical
--     functions, we'll call those 'Odd' pairs.
--
-- What's interesting is that once we've identified a pair's quality,
-- not only will it never change, but we don't actually care about
-- what the operations are anymore!
--
-- So a valid strategy for the @B=20@ case would be to:
--
-- 1. identify the pair's quality for pairs 1 to 5 (counterpart
-- indices 16 to 20).  One query per bit, two queries per pair, that's
-- a complete first block of queries before the 'QuantumFluctuation'.
--
-- 2. identify the pair's quality for remaining pairs 6 to 10.  That's
-- another ten queries and another 'QuantumFluctuation'.
--
-- 3. identify each pair's current shuffling status.  Since we now
-- know each pair's quality, this can be done in a single query per
-- pair.
--
-- 4. We're not querying anymore, so there's no next shuffle and we
-- can output the complete database contents.

-- | The 'PairType' represents a 'Pair'\'s quality, depending on its
-- relationship to its counterpart.
data PairType
  = Even -- ^ pairs are those where the counterpart is equal; such
         -- pairs are unaffected by reversals.
  | Odd  -- ^ pairs are those where the counterpart is the negation;
         -- database reversals and complements have the same effect on
         -- such pairs.

-- | Identify the 'PairType' from a pair of 'Bool's.
pairType :: Bool -> Bool -> PairType
pairType x y | x == y = Even
             | x /= y = Odd

-- | The 'Pair' type represents a database bit and its symmetrical
-- counterpart.  For the bit in the first half of the database:
data PairF bool = Pair {
    pairIdx :: Int    -- ^ remember its index
  , pairValue :: bool -- ^ remember its value
  } deriving Functor -- ^ a hacky derived instance to get 'fmap' at
                     -- little cost
type Pair = PairF Bool
-- ^ I'm lazily using a parametric @bool@ so I can @DeriveFunctor@ and
-- have a free 'fmap' on the relevant payload, namely the 'pairValue'.
-- In this code, it's only ever going to be used as a 'PairF' 'Bool',
-- hence the type synonym.

-- | In addition to `pairIdx` which is a record accessor, `pairIdxC`
-- returns the pair's symmetrical counterpart's index.
pairIdxC :: BitWidth -> Pair -> Int
pairIdxC (BitWidth b) p = b + 1 - pairIdx p

-- | In addition to `pairValue` which is a record accessor,
-- `pairValueC` returns the pair's symmetrical counterpart's value.
pairValueC :: PairType -> Pair -> Bool
pairValueC Even = pairValue
pairValueC Odd = not . pairValue

-- $batch
--
-- The @B=100@ case is going to require more fine-grained information
-- management.
--
-- The insight here is that since the database remains still within a
-- query block, all the pairs read in that timeframe will remain the
-- same with respect to each other, provided they're of the same
-- quality.  For example, with respect to any given bit of a known
-- even pair, the other bits of known even pairs will be have the same
-- value or have the opposite value, but __this will remain true__
-- __after 'QuantumFluctuation's independently of the bits\'__
-- __subsequent value__.
--
-- This is better than having to probe every single pair in a single
-- query block, but it still doesn't cut it for a direct aproach:
-- supposing we'd qualified all 50 pairs of the database, we'd still
-- need to identify each group's shuffle status within a single query
-- block.  But that won't fit: we could have as many as 20 groups (two
-- qualities per block), while we've got the bandwidth to probe only
-- 10 of them.
--
-- So instead of dedicating all of our queries per block to pair
-- qualifying, we'll instead use the first one or two to probe our
-- previous groups' status right after a 'QuantumFluctuation'.  This
-- way, our qualified groups won't be independent from one another
-- anymore, we'll be able to batch them together into only a single
-- batch per pair quality for the entire database.
--
-- Does this fit within the allowable query count?  After the initial
-- block, we'd use up 2 queries to probe, and the remaining 8 to
-- qualify new pairs.  So we cover 8 bits per block, the full 100
-- within 13 blocks.  That's 130 queries or less, it fits!

-- | A `Batch` groups together 'Pair's of a same known (externally)
-- 'PairType'.  If we successfully manage to track one of the batch's
-- representatives' value between 'QuantumFluctuation's, we're able to
-- deduce all the batch's other pairs with no further costly
-- information retrieval!
--
-- The @offset@ parameter enables us to explicitly mark a batch's
-- knowledge as outdated, so we don't accidentally miss a
-- 'QuantumFluctuation' and go out of sync.
data Batch offset
  = Empty -- ^ An empty batch.  Note that those never hold an @offset@.
  | Batch offset (NonEmpty Pair) -- ^ A non-empty batch.
  deriving Functor -- ^ I use the same @DeriveFunctor@ trick, this
                   -- time less idiomatically as the @offset@ can't
                   -- really be considered the payload: this one makes
                   -- for a very easy 'Batch' 'clear'ing
                   -- implementation.

-- | A 'FloatingBatch' is one whose pairs' values we currently don't
-- know, because a 'QuantumFluctuation' happened and we haven't
-- synchronized yet.
type FloatingBatch = Batch ()

-- | A 'BoundBatch' is one whose pairs' values are currently known.
-- To avoid having to update all of the values at each
-- 'QuantumFluctuation', we store this as a 'Bool' to be 'xor'ed with
-- them.
type BoundBatch = Batch Bool

-- | Bind a floating batch to a specific boolean offset.  This
-- consumes up to one query.
bind :: (MonadError QuantumFluctuation m,MonadState Int m,MonadIO m)
     => FloatingBatch -> m BoundBatch
bind Empty = pure Empty
bind (Batch () ps@(p :| _)) = do v <- managedReadAt (pairIdx p)
                                 pure (Batch (v `xor` pairValue p) ps)

-- | Loosen a bound batch back to a floating one.  To be used when we
-- know it'll expire before the next query returns.
clear :: BoundBatch -> FloatingBatch
clear = fmap (const ())

-- | Insert a pair in a batch.  Can only by done if the batch is
-- currently bound.
insert :: Pair -> BoundBatch -> BoundBatch
insert p Empty = Batch False (pure p)
insert p (Batch b ps) = Batch b ((fmap (xor b) p) <| ps)

-- | Expand a batch to a list of @(Int,Bool)@ pairs.  Can only be done
-- if the batch is currently bound.
assocs :: BitWidth -> PairType -> BoundBatch -> [(Int,Bool)]
assocs _ _ Empty = []
assocs bw t (Batch b ps) = concatMap f ps where
  f p = [ (pairIdx p,pairValue p `xor` b)
        , (pairIdxC bw p,pairValueC t p `xor` b) ]

-- $robustM
--
-- To guard against losing track of where I am between two
-- 'QuantumFluctuation's, I'll wrap the database querying with a
-- resource checker, that checks whether the requested query would
-- trigger a reshuffle.  This is implemented with two monad
-- transformers and associated classes:
--
-- * a 'MonadState' 'Int' to count how many queries we performed since
--   the last 'QuantumFluctuation'
--
-- * a 'MonadError' 'QuantumFluctuation' to signal the special
--   condition

-- | Safely query a bit from the database.  If querying now would
-- cause a 'QuantumFluctuation', report it using the 'MonadError'
-- interface instead.
managedReadAt :: (MonadError QuantumFluctuation m,MonadState Int m,MonadIO m)
              => Int -> m Bool
managedReadAt i = get >>= \case 10 -> throwError QuantumFluctuation
                                _  -> modify (+1) *> liftIO (unsafeReadAt i)

-- | The singleton event type to signal.
data QuantumFluctuation = QuantumFluctuation

-- | Query a pair of bits from the database and classify it.
readPair :: ( MonadError QuantumFluctuation m, MonadState Int m
            , MonadReader BitWidth m, MonadIO m )
         => Int -> m (PairType,Pair)
readPair i = do
  BitWidth b <- ask
  x <- managedReadAt i
  y <- managedReadAt (b+1-i)
  pure (if x == y then Even else Odd,Pair { pairIdx = i, pairValue = x })

-- $robustM2
--
-- For reference, the other constraints we encounter in the type
-- signatures:
--
--   * 'MonadReader' 'BitWidth' stores the global constant @B@.
--
--   * 'MonadIO' is a telltale indicator that the affected functions
--     end up interacting with the judge.  If I find the time and
--     don't get frustrated by too much fighting against Google Code
--     Jam's antique GHC version, I'd like to extract the protocol to
--     a higher level of safety, and likely replace this with a free
--     monad.

-- $robustN
--
-- Additionally, some @newtype@s to embellish the type signatures and
-- prevent some classes of variable mixup:

newtype BitWidth = BitWidth Int

-- | Turn an unordered list of indexed booleans from various batches
-- back into a nice string.
reconstruct :: [(Int,Bool)] -> String
reconstruct = map (intToDigit . fromEnum . snd) . sort

-- | Perform the Code Jam judge I/O and tie the high-level pieces
-- together.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [t,b] <- map read . words <$> getLine
  let bw = BitWidth b
  replicateM_ t $ do
    (evens,odds) <- runReaderT (readBlocks Empty Empty [1..b `div` 2]) bw
    putStrLn $ reconstruct $ assocs bw Even evens ++ assocs bw Odd odds
    "Y" <- checkLine  -- still not legal to end a void'ened
    pure undefined    -- block on a monadic pattern bind :-(

-- | Perform a block of queries, maintaining a knowledge base of pair
-- batches between two quantum fluctuations.
readBlocks :: (MonadFail m,MonadReader BitWidth m,MonadIO m)
           => FloatingBatch -> FloatingBatch -> [Int]
           -> m (BoundBatch,BoundBatch)
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
readPairs :: (MonadState Int m,MonadReader BitWidth m,MonadIO m)
          => BoundBatch -> BoundBatch -> [Int]
          -> m (BoundBatch,BoundBatch,Maybe [Int])
readPairs evens odds [] = pure (evens,odds,Nothing)
readPairs evens odds is@(i:is') = runExceptT (readPair i) >>= \case
  Right (Even,p) -> readPairs (insert p evens) odds is'
  Right (Odd,p)  -> readPairs evens (insert p odds) is'
  Left QuantumFluctuation -> pure (evens,odds,Just is)

-- | Read a line from the judge.
--
-- As per protocol, if the line to be returned is an @\"N\"@, that's
-- an interaction-terminating signal as far as the judge is concerned,
-- whether they're caused my a protocol error or a wrong answer.  So
-- exit cleanly ('exitFailure') on those so the judge can return the
-- correct “wrong answer” result instead of “time limit exceeded”.
--
-- IMHO this is a bit lame from the organizers' part, they'd be
-- perfectly able to distinguish those without making the protocol any
-- more cumbersome than it already is.
checkLine :: IO String
checkLine = getLine >>= \case "N" -> exitFailure
                              s   -> pure s

-- | Query a bit from the database.  This is the raw protocol
-- operation, I'm labelling it as unsafe because we don't really know,
-- from the caller's point of view, if a quantum fluctuation is liable
-- to happen.
unsafeReadAt :: Int -> IO Bool
unsafeReadAt i = print i *>
                 checkLine >>= \case "0" -> pure False
                                     "1" -> pure True

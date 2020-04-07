{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
-- Description: ESAb ATAd
-- Copyright: © 2020 Jean-Baptiste Mazon
-- License: GPL-3
-- Stability: experimental
--
-- =Introduction
--
-- This is my solution to Google Code Jam 2020's Qualifier problem D
-- “ESAb ATAd”.  It started (and was submitted as) a very messy piece
-- of code, that really only worked because I'd tested it extensively.
-- Syntactically, the main “loop” was a hairy nest of pattern matches
-- and guards that made it very tricky to understand what was going
-- on, let alone why.  Check out my github if really you
-- [must](https://github.com/jmazon/codejam/blob/submissions/2020-0/D-esab-atad.hs)
-- know what I'm talking about.
--
-- So I refactored it.  Little by little.  Using various Haskell
-- common practices to make bug introduction and reappearance less
-- likely.
--
-- The biggest game changer is the new “batch” type representation I
-- use for knowledge management, which makes it much more clear where
-- in the amnesia process we are.  Next up is implementing query
-- counting as a ressource monad with exit by exception.
--
-- So it's now in a state where it's remarkably overengineered for a
-- throwaway competitive coding problem, yet still (I hope) works on
-- the venerable platform GCJ provides.  Also somewhere in that
-- uncanny valley between literate Haskell and a well-documented
-- module.  More could be done to make it even safer, but not too
-- easily while keeping that platform requirement.  Indexed monads
-- come to mind, also (not as strongly as they're much easier to
-- reimplement) free monads.
--
-- Anyway, now it's done, it might as well be put out there; by chance
-- it could be of interest to someone, be it on the competitive
-- algorithms or the language side.
--
-- == Reading this file
--
-- This file is a [Haddock](https://www.haskell.org/haddock/) module
-- documentation page.  It is not literary Haskell.  Come to think of
-- it, it probably should have been.  But it currently is not.
--
-- Most of it reads as text, with the relevant function signatures
-- interspersed.  That's most of the content anyway.  The source code
-- itself is only a click away: either the “Source” link at the top of
-- this page, or any or the source links on the functions.

module Main (
  -- * The problem
  -- $quanta
  solve10

  -- * The Pair idiom
  -- $pair

  -- ** Pair quality
  , PairType(..), pairType

  -- ** Pair data
  , PairF(..), Pair, pairIndexL, pairIndexR, pairValueR, expand

  -- ** B = 20
  , solve20

  -- * The Batch idiom
  -- $batch
  , Batch(..)

  -- ** Batch subtypes
  , FloatingBatch, BoundBatch, bind, float

  -- ** Container interface
  , insert, assocs

  -- ** B = 100
  , readPairs, readBlocks, solve100

  -- * Introducing robustness
  -- ** Monad transformers
  -- $robustM
  , readBit, QuantumFluctuation, readPair
  -- $robustM2

  -- ** Newtypes
  -- $robustN
  , BitWidth, Index, HalfIndex, halfToFull, halfToFullR, QueryCount

  -- * Protocol
  , reconstruct
  , rawReadBit, provideAnswer, checkLine
  , main

  -- * Closing Thoughts
  -- $closing
  ) where

-- base
import Control.Applicative
import Data.Bits (xor)
import Data.Coerce
import Data.List hiding (insert)
import Data.List.NonEmpty (NonEmpty((:|)),(<|))
import Data.Functor
import System.Exit (exitFailure)
import System.IO
-- mtl
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except

-- MonadFail not really needed for GCJ's rusty GHC (too old);
-- Control.Monad.Fail not needed with recent GHCs.
-- So this import is just the worst of worse world because I didn't
-- figure out a simple enough way of running an old haddock.
import Control.Monad.Fail

-- $quanta
--
-- I won't reproduce the entire statement, you can find it at [on the
-- Code Jam
-- site.](https://codingcompetitions.withgoogle.com/codejam/round/000000000019fd27/0000000000209a9e)
-- I'm never too sure how stable links like that are.  If in doubt,
-- find the Google Code Jam home page using a search engine of your
-- choice, search for past problems in the 2020 qualification round of
-- the Code Jam contest, problem D.
--
-- For a summary: in this interactive problem, the judge has knowledge
-- of a @B@-bits wide word called “the database” (for @B@ among: 10, 20
-- or 100).  It provides us with an operation to query a bit at an
-- index of our choosing.  The goal of the problem is to gain enough
-- knowledge about the database's contents that we can provide the
-- entire bitstring at once.
--
-- There's a catch.  We're only allowed 150 queries.  There's another
-- catch.  Querying the database, one time out of ten, will cause the
-- database's contents to both switch endianness (50% probability) and
-- flip the bits (50% independent probability) before returning the
-- (new) result.
--
-- The saving grace is that we know /which/ queries trigger that
-- so-called /quantum fluctuation/: they're the very first one and
-- then repeat with a period of 10.
--
-- That 'QuantumFluctuation' thing is quite the downer.  We can only
-- read 10 bits at a time from the database before everything is
-- shuffled!
--
-- As it turns out, that's not too much of a problem for the easy
-- @B=10@ case: the bits may be fluctuated before we even start, but
-- as long as we only query 10 of them, they won't be fluctuated
-- again, so our gained knowledge is fresh enough that we can output
-- the result while it's still correct.

-- | Solve a @B=10@ problem case.
solve10 :: ReaderT BitWidth IO ()
solve10 = void $ runExceptT $ flip runStateT (QueryCount 0) $ do
  bits <- mapM (\i -> fmap (i,) (readBit i)) [Index 1 .. Index 10]
  liftIO $ provideAnswer $ reconstruct bits

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
-- of the database, the one it would switch places with if the
-- database were reversed.  From a starting position, we can boil it
-- down to two simple cases:
--
--   * The bits start equal.  In this case, they'll always remain
--     equal, as both operations preserve that.  To track them, all we
--     need is either of them's value, and we automatically know the
--     other one's without having to query it.  As a parallel to
--     analytical functions, we'll call these 'Even' pairs.
--
--   * The bits start diferent.  In that case, they'll always remain
--     different, as both operations preserve that too.  (It may take
--     just a little more thinking to convince yourself of this, but
--     all there is to it is realizing that both operations' start and
--     end pair are the same.)  Same as before, all we need to track
--     them is either of their bit's value.  As a parallel to
--     analytical functions, we'll call those 'Odd' pairs.
--
-- What's interesting is that once we've identified a pair's quality,
-- not only will it never change, but we don't actually care about
-- what the operations are anymore!
--
-- So a valid strategy for the medium @B=20@ case would be to:
--
-- 1. Identify the pair's quality for pairs 1 to 5 (counterpart
-- indices 16 to 20).  One query per bit, two queries per pair, that's
-- a complete first block of queries before the 'QuantumFluctuation'.
--
-- 2. Identify the pair's quality for remaining pairs 6 to 10.  That's
-- another ten queries and another 'QuantumFluctuation'.
--
-- 3. Identify each pair's current fluctuation status.  Since we now
-- know each pair's quality, this can be done in a single query per
-- pair.
--
-- 4. We're not querying anymore, so there's no next fluctuation and
-- we can output the complete database contents.

-- | Represent a t'Pair'\'s quality, depending on its relationship to
-- its counterpart.
data PairType
  = Even -- ^ pairs are those where the counterpart is equal; such
         -- pairs are unaffected by reversals.
  | Odd  -- ^ pairs are those where the counterpart is the negation;
         -- database reversals and complements have the same effect on
         -- such pairs.

-- | Identify the 'PairType' from a given pair of 'Bool's.
pairType :: Bool -> Bool -> PairType
pairType x y | x == y = Even
             | x /= y = Odd

-- | The v'Pair' type represents a database bit and its symmetrical
-- counterpart.  For the bit in the first half of the database:
data PairF bool = Pair {
    pairIndex :: HalfIndex   -- ^ remember its index
  , pairValue :: bool        -- ^ remember its value
  } deriving Functor -- ^ a hacky derived instance to get 'fmap' at
                     -- little cost
type Pair = PairF Bool
-- ^ I'm lazily using a parametric @bool@ so I can @DeriveFunctor@ and
-- have a free 'fmap' on the relevant payload, namely the 'pairValue'.
-- In this code, it's only ever going to be used as a 'PairF' 'Bool',
-- hence the type synonym.

-- | Return a pair's left index.
pairIndexL :: Pair -> Index
pairIndexL = halfToFull . pairIndex

-- | Return a pair's right index.
pairIndexR :: BitWidth -> Pair -> Index
pairIndexR bw = halfToFullR bw . pairIndex

-- | In addition to `pairValue` which is a record accessor, pairValueR
-- returns the pair's symmetrical counterpart's value.
pairValueR :: PairType -> Pair -> Bool
pairValueR Even = pairValue
pairValueR Odd = not . pairValue

-- | Expand a pair back to its two known indexed bit values.
expand :: BitWidth -> PairType -> Pair -> [(Index,Bool)]
expand bw pt p = [ (pairIndexL p,pairValue p)
                 , (pairIndexR bw p,pairValueR pt p) ]

-- | Solve the @B=20@ case by qualifying each pair, then probing all
-- of them in a single block.
solve20 :: ReaderT BitWidth IO ()
solve20 = do
  let block = runExceptT . flip evalStateT (QueryCount 0)
  Right half1 <- block $ mapM readPair [HalfIndex 1 .. HalfIndex 5]
  Right half2 <- block $ mapM readPair [HalfIndex 6 .. HalfIndex 10]
  Right bits  <- block $ mapM readBit  [Index 1 .. Index 10]
  bw <- ask
  let (cs1,ps1) = unzip half1
      (cs2,ps2) = unzip half2
      currentHalf = zipWith ($>) (ps1 ++ ps2) bits
      bits' = concat $ zipWith (expand bw) (cs1 ++ cs2) currentHalf
  liftIO $ provideAnswer $ reconstruct bits'

-- $batch
--
-- The hard @B=100@ case is going to require more fine-grained
-- information management.
--
-- The insight here is that since the database remains still within a
-- query block, all the pairs read in that timeframe will remain the
-- same with respect to each other, provided they're of the same
-- quality.  For example, with respect to any given bit of a known
-- even pair, the other bits of known even pairs will either have the
-- same value or the opposite value, and that fact __will remain__
-- __true after 'QuantumFluctuation's independently of the bits\'__
-- __subsequent value__.
--
-- This is better than having to probe every single pair in a single
-- query block, but it still doesn't cut it for a direct aproach:
-- supposing we'd qualified all 50 pairs of the database, we'd still
-- need to identify each group's fluctuation status within a single
-- query block.  But that won't fit: we could have as many as 20
-- groups (two qualities per block), while we've got the bandwidth to
-- probe only 10 of them.
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

-- | A Batch groups together t'Pair's of a same known (externally)
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
                   -- for a very easy batch 'float'ing implementation.
                   -- I'm ashamed of this one.

-- | A __floating__ batch is one whose pairs' values we currently
-- don't know, because a 'QuantumFluctuation' happened and we haven't
-- synchronized yet.
type FloatingBatch = Batch ()

-- | A __bound__ batch is one whose pairs' values are currently known.
-- To avoid having to update all of the values at each
-- 'QuantumFluctuation', we store this as a 'Bool' to be 'xor'ed with
-- them.
type BoundBatch = Batch Bool

-- | Bind a floating batch to a specific boolean offset.  This
-- consumes up to one query.
bind :: (MonadError QuantumFluctuation m,MonadState QueryCount m,MonadIO m)
     => FloatingBatch -> m BoundBatch
bind Empty = pure Empty
bind (Batch () ps@(p :| _)) = do v <- readBit (halfToFull (pairIndex p))
                                 pure (Batch (v `xor` pairValue p) ps)

-- | Loosen a bound batch back to a floating one.  To be used when we
-- know it'll expire before the next query returns.
float :: BoundBatch -> FloatingBatch
float = fmap (const ())

-- | Insert a pair in a batch.  Can only by done if the batch is
-- currently bound.
insert :: Pair -> BoundBatch -> BoundBatch
insert p Empty = Batch False (pure p)
insert p (Batch b ps) = Batch b ((fmap (xor b) p) <| ps)

-- | Expand a batch to a list of @(Index,Bool)@ pairs.  Can only be done
-- if the batch is currently bound.
assocs :: MonadReader BitWidth m => PairType -> BoundBatch -> m [(Index,Bool)]
assocs _ Empty = pure []
assocs pt (Batch b ps) = do
  bw <- ask
  pure (concatMap (expand bw pt . fmap (xor b)) ps)

-- | Solve the @B=100@ case.  Actually, this would solve any (lower)
-- case, but 'main' currently only calls it in that case.
solve100 :: ReaderT BitWidth IO ()
solve100 = do
  BitWidth bw <- ask
  let pairRange = [HalfIndex 1 .. HalfIndex (bw `div` 2)]
  (evens,odds) <- readBlocks Empty Empty pairRange
  bits <- liftA2 (++) (assocs Even evens) (assocs Odd odds)
  liftIO $ provideAnswer $ reconstruct bits

-- $robustM
--
-- To guard against losing track of where I am between two
-- 'QuantumFluctuation's, I'll wrap the database querying with a basic
-- resource manager, that checks whether the requested query would
-- trigger a fluctuation.  This is implemented with two monad
-- transformers and associated classes:
--
-- * a 'MonadState' 'QueryCount' to count how many queries we
--   performed since the last fluctuation.
--
-- * a 'MonadError' 'QuantumFluctuation' to signal the special
--   condition.
--
-- This helps ensure two things: we only ever trigger fluctuation
-- mitigation if we actually need to perform more queries (see the
-- @B=10@ case); and we don't accidentally sync at a likely mistake
-- point.  After 9 or 11 queries, for example.
--
-- A lot of this would be better-suited to an effects system, but
-- @mtl@ is all we have on the platform.  (And let's consider
-- ourselves lucky.  Or year or two ago, we only had bare
-- @transformers@!)

-- | Safely query a bit from the database.  If querying now would
-- cause a 'QuantumFluctuation', report it using the 'MonadError'
-- interface instead.
readBit :: (MonadError QuantumFluctuation m,MonadState QueryCount m,MonadIO m)
        => Index -> m Bool
readBit i = get >>= \case
  QueryCount 10 -> throwError QuantumFluctuation
  _             -> modify succ *> liftIO (rawReadBit i)

-- | The singleton event type to signal when bad things are happening.
data QuantumFluctuation = QuantumFluctuation

-- | Query a pair of bits from the database and classify it.
readPair :: ( MonadError QuantumFluctuation m, MonadState QueryCount m
            , MonadReader BitWidth m, MonadIO m)
            => HalfIndex -> m (PairType,Pair)
readPair i = do
  bw <- ask
  x <- readBit (halfToFull i)
  y <- readBit (halfToFullR bw i)
  pure (if x == y then Even else Odd,Pair { pairIndex = i, pairValue = x })

-- $robustM2
--
-- For reference, the other constraints we encounter in the type
-- signatures are:
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

-- | A wrapper around problem-global variable @B@.  Converting to this
-- earlier would have spared me quite a few mix-ups between identifier
-- @b@ referring to the database width or identifier @b@ referring to
-- a generic bit/boolean value.
--
-- On the one hand I could just use more verbose identifiers.  On the
-- other hand, having the typesystem help is always good.
newtype BitWidth = BitWidth Int

-- | A wrapper around an index to the database.  Range from @1@ to @B@.
newtype Index = Index Int deriving ( Eq   -- ^ needed for 'Ord'
                                   , Ord  -- ^ needed to sort in 'reconstruct'
                                   , Enum -- ^ needed for the easy case agenda
                                   , Num  -- ^ needed to convert counterparts
                                   )

-- | A wrapper around an index to the first half of the database.
-- Range @1@ to @B/2@.
newtype HalfIndex = HalfIndex Int deriving Enum -- ^ needed for the agenda

-- | Conversion from a half-index to a full one is always safe.
halfToFull :: HalfIndex -> Index
halfToFull = coerce

-- | Conversion from a half-index to the full one of its right part
-- requires knowing @B@.
halfToFullR :: BitWidth -> HalfIndex -> Index
halfToFullR (BitWidth bw) = (Index bw+1 -) . halfToFull

-- | A wrapper around the query count for “managed” querying.
newtype QueryCount = QueryCount Int deriving ( Eq   -- ^ check for limit
                                             , Enum -- ^ increase
                                             )

-- | Perform a block of queries, maintaining a knowledge base of pair
-- batches between two quantum fluctuations.
readBlocks :: (MonadFail m,MonadReader BitWidth m,MonadIO m)
           => FloatingBatch -> FloatingBatch -> [HalfIndex]
           -> m (BoundBatch,BoundBatch)
readBlocks ftEvens ftOdds indices = do
  (bdEvens',bdOdds',mbIndices') <- flip evalStateT (QueryCount 0) $ do
    -- Despite the MonadFail instance, the first two queries can't
    -- fail since the query count is 0 then 1 at this time.  Guarding
    -- against this statically without making the code three times as
    -- long reaches beyond what we have avilable on the GCJ platform.
    Right bdEvens <- runExceptT (bind ftEvens)
    Right bdOdds  <- runExceptT (bind ftOdds)
    readPairs bdEvens bdOdds indices
  case mbIndices' of
    Just indices' -> readBlocks (float bdEvens') (float bdOdds') indices'
    Nothing       -> pure (bdEvens',bdOdds')

-- | Read, classify and store pairs from the database until the next
-- query would result in a quantum fluctuation.
readPairs :: (MonadState QueryCount m,MonadReader BitWidth m,MonadIO m)
          => BoundBatch -> BoundBatch -> [HalfIndex]
          -> m (BoundBatch,BoundBatch,Maybe [HalfIndex])
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

-- | Turn an unordered list of indexed booleans from various batches
-- back into a nice bitstring.
reconstruct :: [(Index,Bool)] -> [Bool]
reconstruct = map snd . sort

-- | Query a bit from the database.  This is the raw protocol
-- operation.
--
-- I used to label is as ‘unsafe’ to signal not to use it directly,
-- but I've since then written the easier variations of this puzzle,
-- so I'm now going with ‘raw’.
rawReadBit :: Index -> IO Bool
rawReadBit (Index i) = print i *>
                      checkLine >>= \case "0" -> pure False
                                          "1" -> pure True

-- | Provide an answer to the judge.
provideAnswer :: Foldable f => f Bool -> IO ()
provideAnswer answer = do
  putStrLn $ concatMap (show . fromEnum) answer
  "Y" <- checkLine  -- still not legal to end a void'ened
  pure undefined    -- block on a monadic pattern bind :-(

-- | Perform the Code Jam judge I/O and tie the high-level pieces
-- together.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [t,b] <- map read . words <$> getLine
  let solver = case b of 10  -> solve10
                         20  -> solve20
                         100 -> solve100
  replicateM_ t $ runReaderT solver (BitWidth b)

-- $closing
--
-- Despite the current GCJ interface and level of Haskell, this
-- problem was a very interesting one to solve.
--
-- If I find the motivation to put some more time into this, there's
-- more to be done on a few fronts:
--
--   * The main algorithm handles two batches: one for each pair
--     quality.  This is horrible!  We could use one instead of the
--     other by acident and come up with wrong results!  This could
--     easily be solved by integrating the quality in the pair.  But
--     that's very wasteful, and some kind of an invitation to store
--     pairs in the wrong batch.  We could instead store it in the
--     batch itself.  But that's still a bit of a waste, as we are
--     going to have exactly two of them every time anyway.  I'm
--     considering a phantom type parameter for that, but I'm still
--     not sure how to integrate it without too much unpleasantness.
--
--   * My implementation of the judge I/O protocol still has two
--     weak spots:
--
--       - Some of the calls to 'readBit' are marked as failable, yet
--         we know they can't since they're the first of a query
--         block.  Solving that would require oh-so-many language
--         extensions, but seems doable.  Just really out of touch for
--         competition code.  For now.
--
--       - There's no static limit on the 150 total queries yet.
--
--   * The 'Index'es are newtyped and some care is taken when
--     converting, but they're not statically bound by @B@ either, and
--     this also could result in protocol failures.
--
-- Feedback, comments and suggestions welcome.  Reasonable
-- improvements too!

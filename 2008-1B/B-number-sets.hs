{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, FlexibleInstances,
    MultiParamTypeClasses, UndecidableInstances, RecursiveDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP, MagicHash, Rank2Types #-}

-- Control.Monad.ST.Trans
import GHC.Base hiding (mapM,join)
import GHC.Arr (Ix(..), Array(..))
import qualified GHC.Arr as STArray
import Data.STRef (STRef)
import qualified Data.STRef as STRef
import Data.Array.ST hiding (runSTArray)
import Data.IORef
import Unsafe.Coerce
import System.IO.Unsafe

import Data.Array.Base
import GHC.Int    (      Int8,  Int16,  Int32,  Int64)
import GHC.Word   (Word, Word8, Word16, Word32, Word64)
import GHC.Ptr    (Ptr, FunPtr)
import GHC.Stable (StablePtr)

-- .Internal
import GHC.ST hiding (liftST,runST)
import qualified Control.Monad.Fail as MF
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

-- Data.Equivalence.STT
import Control.Monad hiding (join)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

-- proper
import qualified Data.IntSet as S
import Control.Monad.Identity hiding (mapM,join)
import System.IO

solve :: (Int,Int,Int) -> Int
solve (a,b,p) = runIdentity $ runST $ do
  e <- leastEquiv id min
  forM_ (takeWhile (<= b-a) $ dropWhile (<p) primes) $ \p' -> do
    let n = (a+p'-1) `div` p' * p'
    equateAll e [n,n+p'..b]
  fmap (S.size . S.fromList) (mapM (classDesc e) [a..b])

main :: IO ()
main = hSetBuffering stdout LineBuffering >> (interact $ unlines . zipWith c [1 :: Int ..] . map solve . parse . map read . tail . words)
  where c i j = "Case #" ++ show i ++ ": " ++ show j

parse :: [Int] -> [(Int,Int,Int)]
parse (a:b:p:ns) = (a,b,p) : parse ns
parse _ = []

-- success, but 44 minutes on large, compiled and optimized

------------------------------------------------------------
-- Control.Monad.ST.Trans

{- |
   Module      :  Control.Monad.ST.Trans
   Copyright   :  Josef Svenningsson 2008-2017
                  (c) The University of Glasgow, 1994-2000
   License     :  BSD
 
   Maintainer  :  josef.svenningsson@gmail.com
   Stability   :  experimental
   Portability :  non-portable (GHC Extensions)

   This library provides a monad transformer version of the ST monad.

   Warning! This monad transformer should not be used with monads that
   can contain multiple answers, like the list monad. The reason is that 
   the state token will be duplicated across the different answers and
   this causes Bad Things to happen (such as loss of referential
   transparency). Safe monads include the monads State, Reader, Writer,
   Maybe and combinations of their corresponding monad transformers.

-}

{-# INLINE newSTRef #-}
-- | Create a new reference
newSTRef :: (Applicative m) => a -> STT s m (STRef s a)
newSTRef i = liftST (STRef.newSTRef i)

{-# INLINE readSTRef #-}
-- | Reads the value of a reference
readSTRef :: (Applicative m) => STRef s a -> STT s m a
readSTRef ref = liftST (STRef.readSTRef ref)

{-# INLINE writeSTRef #-}
-- | Modifies the value of a reference
writeSTRef :: (Applicative m) => STRef s a -> a -> STT s m ()
writeSTRef ref a = liftST (STRef.writeSTRef ref a)

{-# DEPRECATED runST "Use runSTT instead" #-}
{-# NOINLINE runST #-}
-- | Executes a computation in the 'STT' monad transformer
runST :: Monad m => (forall s. STT s m a) -> m a
runST m = let (STT f) = m
 -- the parenthesis is needed because of a bug in GHC's parser
          in do (STTRet _st a) <- ( f realWorld# )
                return a

{-# NOINLINE runSTT #-}
-- | Executes a computation in the 'STT' monad transformer
runSTT :: Monad m => (forall s. STT s m a) -> m a
runSTT m = let (STT f) = m
           in do (STTRet _st a) <- ( f realWorld# )
                 return a

-- Mutable arrays.

{-# INLINE newSTArray #-}
-- | Creates a new mutable array
newSTArray :: (Ix i, Applicative m) =>
              (i,i) -> e -> STT s m (STArray s i e)
newSTArray bnds i = liftST (newArray bnds i)

{-# INLINE boundsSTArray #-}
-- | Returns the lowest and highest indices of the array
boundsSTArray :: STArray s i e -> (i,i)
boundsSTArray = STArray.boundsSTArray

{-# INLINE numElementsSTArray #-}
-- | Returns the number of elements in the array
numElementsSTArray :: STArray s i e -> Int
numElementsSTArray = STArray.numElementsSTArray

{-# INLINE readSTArray #-}
-- | Retrieves an element from the array
readSTArray :: (Ix i, Applicative m) =>
               STArray s i e -> i -> STT s m e
readSTArray arr i = liftST (readArray arr i)

{-# INLINE unsafeReadSTArray #-}
unsafeReadSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
    Applicative m) =>
                     STArray s i e -> Int -> STT s m e
unsafeReadSTArray arr i = liftST (STArray.unsafeReadSTArray arr i)

{-# INLINE writeSTArray #-}
-- | Modifies an element in the array
writeSTArray :: (Ix i, Applicative m) =>
                STArray s i e -> i -> e -> STT s m ()
writeSTArray arr i e = liftST (writeArray arr i e)

{-# INLINE unsafeWriteSTArray #-}
unsafeWriteSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
                      STArray s i e -> Int -> e -> STT s m ()
unsafeWriteSTArray arr i e = liftST (STArray.unsafeWriteSTArray arr i e)

{-# INLINE freezeSTArray #-}
-- | Copy a mutable array and turn it into an immutable array
freezeSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
                 STArray s i e -> STT s m (Array i e)
freezeSTArray arr = liftST (STArray.freezeSTArray arr)

{-# INLINE unsafeFreezeSTArray #-}
unsafeFreezeSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
                       STArray s i e -> STT s m (Array i e)
unsafeFreezeSTArray arr = liftST (STArray.unsafeFreezeSTArray arr)

{-# INLINE thawSTArray #-}
-- | Copy an immutable array and turn it into a mutable array
thawSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
               Array i e -> STT s m (STArray s i e)
thawSTArray arr = liftST (STArray.thawSTArray arr)

{-# INLINE unsafeThawSTArray #-}
unsafeThawSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
    Ix i,
#endif
  Applicative m) =>
                     Array i e -> STT s m (STArray s i e)
unsafeThawSTArray arr = liftST (STArray.unsafeThawSTArray arr)

{-# INLINE runSTArray #-}
-- | A safe way to create and work with a mutable array before returning an
-- immutable array for later perusal.  This function avoids copying
-- the array before returning it.
runSTArray :: (
#if __GLASGOW_HASKELL__ <= 710
  Ix i,
#endif
#if __GLASGOW_HASKELL__ <= 708
  Applicative m,
#endif
  Monad m)
           => (forall s . STT s m (STArray s i e))
           -> m (Array i e)
runSTArray st = runSTT (st >>= unsafeFreezeSTArray)


{-# NOINLINE unsafeIOToSTT #-} 
unsafeIOToSTT :: (Monad m) => IO a -> STT s m a
unsafeIOToSTT m = return $! unsafePerformIO m

{-# DEPRECATED unsafeSTToIO "Use unsafeSTTToIO instead" #-}
unsafeSTToIO :: STT s IO a -> IO a
unsafeSTToIO m = runSTT $ unsafeCoerce m

unsafeSTTToIO :: STT s IO a -> IO a
unsafeSTTToIO m = runSTT $ unsafeCoerce m

-- This should work, as STRef and IORef should have identical internal representation
unsafeSTRefToIORef  :: STRef s a -> IORef a
unsafeSTRefToIORef ref = unsafeCoerce ref

unsafeIORefToSTRef :: IORef a -> STRef s a
unsafeIORefToSTRef ref = unsafeCoerce ref

------------------------------------------------------------
-- Data.Equivalence.STT

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Equivalence.STT
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This is an implementation of Tarjan's Union-Find algorithm (Robert
-- E. Tarjan. "Efficiency of a Good But Not Linear Set Union
-- Algorithm", JACM 22(2), 1975) in order to maintain an equivalence
-- relation. 
-- 
-- This implementation is a port of the /union-find/ package using the
-- ST monad transformer (instead of the IO monad).
--
-- The implementation is based on mutable references.  Each
-- equivalence class has exactly one member that serves as its
-- representative element.  Every element either is the representative
-- element of its equivalence class or points to another element in
-- the same equivalence class.  Equivalence testing thus consists of
-- following the pointers to the representative elements and then
-- comparing these for identity.
--
-- The algorithm performs lazy path compression.  That is, whenever we
-- walk along a path greater than length 1 we automatically update the
-- pointers along the path to directly point to the representative
-- element.  Consequently future lookups will be have a path length of
-- at most 1.
--
-- Each equivalence class remains a descriptor, i.e. some piece of
-- data attached to an equivalence class which is combined when two
-- classes are unioned.
--
--------------------------------------------------------------------------------

newtype Class s c a = Class (STRef s (Entry s c a))


{-| This type represents a reference to an entry in the tree data
structure. An entry of type 'Entry' @s c a@ lives in the state space
indexed by @s@, contains equivalence class descriptors of type @c@ and
has elements of type @a@.-}

newtype Entry s c a = Entry {unentry :: STRef s (EntryData s c a)}

{-| This type represents entries (nodes) in the tree data
structure. Entry data of type 'EntryData' @s c a@ lives in the state space
indexed by @s@, contains equivalence class descriptors of type @c@ and
has elements of type @a@.  -}

data EntryData s c a = Node {
      entryParent :: Entry s c a,
      entryValue :: a
    }
                     | Root {
      entryDesc :: c,
      entryWeight :: Int,
      entryValue :: a,
      entryDeleted :: Bool
    }

type Entries s c a = STRef s (Map a (Entry s c a))

{-| This is the top-level data structure that represents an
equivalence relation. An equivalence relation of type 'Equiv' @s c a@
lives in the state space indexed by @s@, contains equivalence class
descriptors of type @c@ and has elements of type @a@. -}

data Equiv s c a = Equiv {
      -- | maps elements to their entry in the tree data structure
      entries :: Entries s c a,
      -- | constructs an equivalence class descriptor for a singleton class
      singleDesc :: a -> c,
      -- | combines the equivalence class descriptor of two classes
      --   which are meant to be combined.
      combDesc :: c -> c -> c
      }

{-| This function constructs the initial data structure for
maintaining an equivalence relation. That is it represents, the fines
(or least) equivalence class (of the set of all elements of type
@a@). The arguments are used to maintain equivalence class
descriptors. -}

leastEquiv :: (Monad m, Applicative m)
           => (a -> c) -- ^ used to construct an equivalence class descriptor for a singleton class
           -> (c -> c -> c) -- ^ used to combine the equivalence class descriptor of two classes
                            --   which are meant to be combined.
           -> STT s m (Equiv s c a)
leastEquiv mk com = do
  es <- newSTRef Map.empty
  return Equiv {entries = es, singleDesc = mk, combDesc = com}



{-| This function returns the representative entry of the argument's
equivalence class (i.e. the root of its tree) or @Nothing@ if it is
the representative itself.

This function performs path compression.  -}

representative' :: (Monad m, Applicative m) => Entry s c a -> STT s m (Maybe (Entry s c a),Bool)
representative' (Entry e) = do
  ed <- readSTRef e
  case ed of
    Root {entryDeleted = del} -> do
      return (Nothing, del)
    Node {entryParent = parent} -> do
      (mparent',del) <- representative' parent
      case mparent' of
        Nothing -> return $ (Just parent, del)
        Just parent' -> writeSTRef e ed{entryParent = parent'} >> return (Just parent', del)


{-| This function returns the representative entry of the argument's
equivalence class (i.e. the root of its tree).

This function performs path compression.  -}

representative :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m (Entry s c a)
representative eq v = do
  mentry <- getEntry eq v
  case mentry of -- check whether there is an entry
    Nothing -> mkEntry eq v -- if not, create a new one
    Just entry -> do
      (mrepr,del) <- representative' entry
      if del -- check whether equivalence class was deleted
        then mkEntry eq v -- if so, create a new entry
        else case mrepr of
               Nothing -> return entry
               Just repr -> return repr

{-| This function provides the representative entry of the given
equivalence class. This function performs path compression. -}

classRep :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> STT s m (Entry s c a)
classRep eq (Class p) = do
  entry <- readSTRef p
  (mrepr0,del0) <- representative' entry
  if del0 -- check whether equivalence class was deleted
    then do v <- liftM entryValue $ readSTRef (unentry entry)
            en <- getEntry' eq v -- if so, create a new entry
            (mrepr,del) <- representative' en
            if del then do
                en' <- mkEntry' eq en
                writeSTRef p en'
                return en'
              else return (fromMaybe en mrepr)
    else return (fromMaybe entry mrepr0)


{-| This function constructs a new (root) entry containing the given
entry's value, inserts it into the lookup table (thereby removing any
existing entry). -}

mkEntry' :: (Monad m, Applicative m, Ord a)
        => Equiv s c a -> Entry s c a
        -> STT s m (Entry s c a)  -- ^ the constructed entry
mkEntry' eq (Entry e) = readSTRef e >>= mkEntry eq . entryValue

{-| This function constructs a new (root) entry containing the given
value, inserts it into the lookup table (thereby removing any existing
entry). -}

mkEntry :: (Monad m, Applicative m, Ord a)
        => Equiv s c a -> a
        -> STT s m (Entry s c a)  -- ^ the constructed entry
mkEntry Equiv {entries = mref, singleDesc = mkDesc} val = do
  e <- newSTRef Root
       { entryDesc = mkDesc val,
         entryWeight = 1,
         entryValue = val,
         entryDeleted = False
       }
  let entry = Entry e
  m <- readSTRef mref
  writeSTRef mref (Map.insert val entry m)
  return entry

{-| This function provides the equivalence class the given element is
contained in. -}

getClass :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m (Class s c a)
getClass eq v = do
  en <- (getEntry' eq v)
  liftM Class $ newSTRef en


getEntry' :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m (Entry s c a)
getEntry' eq v = do
  mentry <- getEntry eq v
  case mentry of
    Nothing -> mkEntry eq v
    Just entry -> return entry

{-| This function looks up the entry of the given element in the given
equivalence relation representation or @Nothing@ if there is none,
yet.  -}

getEntry :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m (Maybe (Entry s c a))
getEntry Equiv { entries = mref} val = do
  m <- readSTRef mref
  case Map.lookup val m of
    Nothing -> return Nothing
    Just entry -> return $ Just entry



{-| This function equates the two given (representative) elements. That
is, it unions the equivalence classes of the two elements and combines
their descriptor. The returned entry is the representative of the new
equivalence class -}

equateEntry :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Entry s c a -> Entry s c a -> STT s m (Entry s c a)
equateEntry Equiv {combDesc = mkDesc} repx@(Entry rx) repy@(Entry ry) =
  if (rx /= ry) then do
    dx <- readSTRef rx
    dy <- readSTRef ry
    case (dx, dy) of
      ( Root{entryWeight = wx, entryDesc = chx, entryValue = vx}
        , Root{entryWeight = wy, entryDesc = chy, entryValue = vy} ) ->
        if  wx >= wy
        then do
          writeSTRef ry Node {entryParent = repx, entryValue = vy}
          writeSTRef rx dx{entryWeight = wx + wy, entryDesc = mkDesc chx chy}
          return repx
        else do
          writeSTRef rx Node {entryParent = repy, entryValue = vx}
          writeSTRef ry dy{entryWeight = wx + wy, entryDesc = mkDesc chx chy}
          return repy

      _ -> error "error on `equateEntry`"
      -- this should not happen as this function is only called by
      -- 'combineEntries', which always uses representative entries
  else return  repx

combineEntries :: (Monad m, Applicative m, Ord a)
               => Equiv s c a -> [b] -> (b -> STT s m (Entry s c a)) -> STT s m ()
combineEntries  _ [] _ = return ()
combineEntries eq (e:es) rep = do
  er <- rep e
  run er es
    where run er (f:r) = do
            fr <- rep f
            er' <- equateEntry eq er fr
            run er' r
          run _ _ = return ()


{-| This function combines all equivalence classes in the given
list. Afterwards all elements in the argument list represent the same
equivalence class! -}

combineAll :: (Monad m, Applicative m, Ord a) => Equiv s c a -> [Class s c a] -> STT s m ()
combineAll eq cls = combineEntries eq cls (classRep eq)


{-| This function combines the two given equivalence
classes. Afterwards both arguments represent the same equivalence
class! One of it is returned in order to represent the new combined
equivalence class. -}

combine :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> Class s c a -> STT s m (Class s c a)
combine eq x y = combineAll eq [x,y] >> return x


{-| This function equates the element in the given list. That is, it
unions the equivalence classes of the elements and combines their
descriptor. -}

equateAll :: (Monad m, Applicative m, Ord a) => Equiv s c a -> [a] -> STT s m ()
equateAll eq cls = combineEntries eq cls (representative eq)

{-| This function equates the two given elements. That is, it unions
the equivalence classes of the two elements and combines their
descriptor. -}

equate :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> a -> STT s m ()
equate eq x y = equateAll eq [x,y]


{-| This function returns the descriptor of the given
equivalence class. -}

desc :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> STT s m c
desc eq cl = do
  Entry e <- classRep eq cl
  liftM entryDesc $ readSTRef e

{-| This function returns the descriptor of the given element's
equivalence class. -}

classDesc :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m c
classDesc eq val = do
  Entry e <- representative eq val
  liftM entryDesc $ readSTRef e


{-| This function decides whether the two given equivalence classes
are the same. -}

same :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> Class s c a -> STT s m Bool
same eq c1 c2 = do
  (Entry r1) <- classRep eq c1
  (Entry r2) <- classRep eq c2
  return (r1 == r2)

{-| This function decides whether the two given elements are in the
same equivalence class according to the given equivalence relation
representation. -}

equivalent :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> a -> STT s m Bool
equivalent eq v1 v2 = do
  (Entry r1) <- representative eq v1
  (Entry r2) <- representative eq v2
  return (r1 == r2)



{-|
  This function modifies the content of a reference cell.
 -}

modifySTRef :: (Monad m, Applicative m) => STRef s a -> (a -> a) -> STT s m ()
modifySTRef r f = readSTRef r >>= (writeSTRef r . f)


{-| This function marks the given root entry as deleted.  -}

removeEntry :: (Monad m, Applicative m, Ord a) => Entry s c a -> STT s m ()
removeEntry (Entry r) = modifySTRef r change
    where change e = e {entryDeleted = True}


{-| This function removes the given equivalence class. If the
equivalence class does not exists anymore @False@ is returned;
otherwise @True@. -}

remove :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> STT s m Bool
remove eq (Class p) = do
  entry <- readSTRef p
  (mrepr0,del0) <- representative' entry
  if del0 then do
        v <- liftM entryValue $ readSTRef (unentry entry)
        men <- getEntry eq v
        case men of
          Nothing -> return False
          Just en -> do
            writeSTRef p en
            (mentry,del) <- representative' en
            if del
              then return False
              else removeEntry (fromMaybe en mentry)
                   >> return True
    else removeEntry (fromMaybe entry mrepr0)
         >> return True

{-| This function removes the equivalence class of the given
element. If there is no corresponding equivalence class, @False@ is
returned; otherwise @True@. -}

removeClass :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m Bool
removeClass eq v = do
  mentry0 <- getEntry eq v
  case mentry0 of
    Nothing -> return False
    Just entry -> do
      (mentry, del) <- representative' entry
      if del
        then return False
        else removeEntry (fromMaybe entry mentry)
             >> return True

------------------------------------------------------------
-- Data.Numbers.Primes


-- |
-- Module      : Data.Numbers.Primes
-- Copyright   : Sebastian Fischer
-- License     : BSD3
-- 
-- Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
-- 
-- This Haskell library provides an efficient lazy wheel sieve for
-- prime generation inspired by /Lazy wheel sieves and spirals of/
-- /primes/ by Colin Runciman
-- (<http://www.cs.york.ac.uk/ftpdir/pub/colin/jfp97lw.ps.gz>) and
-- /The Genuine Sieve of Eratosthenes/ by Melissa O'Neil
-- (<http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>).
-- 

-- This global constant is an infinite list of prime numbers. It is
-- generated by a lazy wheel sieve and shared across the whole program
-- run. If you are concerned about the memory requirements of sharing
-- many primes you can call the function @wheelSieve@ directly.
-- 
primes :: Integral int => [int]
primes = wheelSieve 6

{-# SPECIALISE primes :: [Int]     #-}
{-# SPECIALISE primes :: [Integer] #-}

-- | 
-- This function returns an infinite list of prime numbers by sieving
-- with a wheel that cancels the multiples of the first @n@ primes
-- where @n@ is the argument given to @wheelSieve@. Don't use too
-- large wheels. The number @6@ is a good value to pass to this
-- function. Larger wheels improve the run time at the cost of higher
-- memory requirements.
-- 
wheelSieve :: Integral int
           => Int    -- ^ number of primes canceled by the wheel
           -> [int]  -- ^ infinite list of primes
wheelSieve k = reverse ps ++ map head (sieve p (cycle ns))
 where (p:ps,ns) = wheel k

{-# SPECIALISE wheelSieve :: Int -> [Int]     #-}
{-# SPECIALISE wheelSieve :: Int -> [Integer] #-}

-- |
-- Checks whether a given number is prime.
-- 
-- This function uses trial division to check for divisibility with
-- all primes below the square root of the given number. It is
-- impractical for numbers with a very large smallest prime factor.
-- 
isPrime :: Integral int => int -> Bool
isPrime n | n > 1     = primeFactors n == [n]
          | otherwise = False

{-# SPECIALISE isPrime :: Int     -> Bool #-}
{-# SPECIALISE isPrime :: Integer -> Bool #-}

-- |
-- Yields the sorted list of prime factors of the given positive
-- number.
-- 
-- This function uses trial division and is impractical for numbers
-- with very large prime factors.
-- 
primeFactors :: Integral int => int -> [int]
primeFactors n = factors n (wheelSieve 6)
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [m]
                   | r == 0    = p : factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

{-# SPECIALISE primeFactors :: Int     -> [Int]     #-}
{-# SPECIALISE primeFactors :: Integer -> [Integer] #-}

-- Auxiliary Definitions
------------------------------------------------------------------------------

-- Sieves prime candidates by computing composites from the result of
-- a recursive call with identical arguments. We could use sharing
-- instead of a recursive call with identical arguments but that would
-- lead to much higher memory requirements. The results of the
-- different calls are consumed at different speeds and we want to
-- avoid multiple far apart pointers into the result list to avoid
-- retaining everything in between.
--
-- Each list in the result starts with a prime. To obtain composites
-- that need to be cancelled, one can multiply all elements of the
-- list with its head.
-- 
sieve :: (Ord int, Num int) => int -> [int] -> [[int]]
sieve p ns@(m:ms) = spin p ns : sieveComps (p+m) ms (composites p ns)

{-# SPECIALISE sieve :: Int     -> [Int]     -> [[Int]]     #-}
{-# SPECIALISE sieve :: Integer -> [Integer] -> [[Integer]] #-}

-- Composites are stored in increasing order in a priority queue. The
-- queue has an associated feeder which is used to avoid filling it
-- with entries that will only be used again much later. 
-- 
type Composites int = (Queue int,[[int]])

-- The feeder is computed from the result of a call to 'sieve'.
-- 
composites :: (Ord int, Num int) => int -> [int] -> Composites int
composites p ns = (Empty, map comps (spin p ns : sieve p ns))
 where comps xs@(x:_) = map (x*) xs

{-# SPECIALISE composites :: Int     -> [Int]     -> Composites Int     #-}
{-# SPECIALISE composites :: Integer -> [Integer] -> Composites Integer #-}

-- We can split all composites into the next and remaining
-- composites. We use the feeder when appropriate and discard equal
-- entries to not return a composite twice.
-- 
splitComposites :: Ord int => Composites int -> (int,Composites int)
splitComposites (Empty, xs:xss) = splitComposites (Fork xs [], xss)
splitComposites (queue, xss@((x:xs):yss))
  | x < z     = (x, discard x (enqueue xs queue, yss))
  | otherwise = (z, discard z (enqueue zs queue', xss))
 where (z:zs,queue') = dequeue queue

{-# SPECIALISE splitComposites :: Composites Int -> (Int,Composites Int) #-}
{-# SPECIALISE
    splitComposites :: Composites Integer -> (Integer,Composites Integer) #-}

-- Drops all occurrences of the given element.
--
discard :: Ord int => int -> Composites int -> Composites int
discard n ns | n == m    = discard n ms
             | otherwise = ns
 where (m,ms) = splitComposites ns

{-# SPECIALISE discard :: Int -> Composites Int -> Composites Int #-}
{-# SPECIALISE
    discard :: Integer -> Composites Integer -> Composites Integer #-}

-- This is the actual sieve. It discards candidates that are
-- composites and yields lists which start with a prime and contain
-- all factors of the composites that need to be dropped.
--
sieveComps :: (Ord int, Num int) => int -> [int] -> Composites int -> [[int]]
sieveComps cand ns@(m:ms) xs
  | cand == comp = sieveComps (cand+m) ms ys
  | cand <  comp = spin cand ns : sieveComps (cand+m) ms xs
  | otherwise    = sieveComps cand ns ys
 where (comp,ys) = splitComposites xs

{-# SPECIALISE sieveComps :: Int -> [Int] -> Composites Int -> [[Int]] #-}
{-# SPECIALISE
    sieveComps :: Integer -> [Integer] -> Composites Integer -> [[Integer]] #-}

-- This function computes factors of composites of primes by spinning
-- a wheel.
-- 
spin :: Num int => int -> [int] -> [int]
spin x (y:ys) = x : spin (x+y) ys

{-# SPECIALISE spin :: Int     -> [Int]     -> [Int]     #-}
{-# SPECIALISE spin :: Integer -> [Integer] -> [Integer] #-}

-- A wheel consists of a list of primes whose multiples are canceled
-- and the actual wheel that is rolled for canceling.
--
type Wheel int = ([int],[int])

-- Computes a wheel that cancels the multiples of the given number
-- (plus 1) of primes.
--
-- For example:
--
-- wheel 0 = ([2],[1])
-- wheel 1 = ([3,2],[2])
-- wheel 2 = ([5,3,2],[2,4])
-- wheel 3 = ([7,5,3,2],[4,2,4,2,4,6,2,6])
--
wheel :: Integral int => Int -> Wheel int
wheel n = iterate next ([2],[1]) !! n

{-# SPECIALISE wheel :: Int -> Wheel Int     #-}
{-# SPECIALISE wheel :: Int -> Wheel Integer #-}

next :: Integral int => Wheel int -> Wheel int
next (ps@(p:_),xs) = (py:ps,cancel (product ps) p py ys)
 where (y:ys) = cycle xs
       py = p + y

{-# SPECIALISE next :: Wheel Int     -> Wheel Int     #-}
{-# SPECIALISE next :: Wheel Integer -> Wheel Integer #-}

cancel :: Integral int => int -> int -> int -> [int] -> [int]
cancel 0 _ _ _ = []
cancel m p n (x:ys@(y:zs))
  | nx `mod` p > 0 = x : cancel (m-x) p nx ys
  | otherwise      = cancel m p n (x+y:zs)
 where nx = n + x

{-# SPECIALISE cancel :: Int -> Int -> Int -> [Int] -> [Int] #-}
{-# SPECIALISE
    cancel :: Integer -> Integer -> Integer -> [Integer] -> [Integer] #-}

-- We use a special version of priority queues implemented as /pairing/
-- /heaps/ (see /Purely Functional Data Structures/ by Chris Okasaki).
--
-- The queue stores non-empty lists of composites; the first element
-- is used as priority.
--
data Queue int = Empty | Fork [int] [Queue int]

enqueue :: Ord int => [int] -> Queue int -> Queue int
enqueue ns = merge (Fork ns [])

{-# SPECIALISE enqueue :: [Int]     -> Queue Int     -> Queue Int     #-}
{-# SPECIALISE enqueue :: [Integer] -> Queue Integer -> Queue Integer #-}

merge :: Ord int => Queue int -> Queue int -> Queue int
merge Empty y                        = y
merge x     Empty                    = x
merge x     y     | prio x <= prio y = join x y
                  | otherwise        = join y x
 where prio (Fork (n:_) _) = n
       join (Fork ns qs) q = Fork ns (q:qs)

{-# SPECIALISE merge :: Queue Int     -> Queue Int     -> Queue Int     #-}
{-# SPECIALISE merge :: Queue Integer -> Queue Integer -> Queue Integer #-}

dequeue :: Ord int => Queue int -> ([int], Queue int)
dequeue (Fork ns qs) = (ns,mergeAll qs)

{-# SPECIALISE dequeue :: Queue Int     -> ([Int],     Queue Int)     #-}
{-# SPECIALISE dequeue :: Queue Integer -> ([Integer], Queue Integer) #-}

mergeAll :: Ord int => [Queue int] -> Queue int
mergeAll []       = Empty
mergeAll [x]      = x
mergeAll (x:y:qs) = merge (merge x y) (mergeAll qs)

{-# SPECIALISE mergeAll :: [Queue Int]     -> Queue Int     #-}
{-# SPECIALISE mergeAll :: [Queue Integer] -> Queue Integer #-}

------------------------------------------------------------
-- Control.Monad.ST.Trans.Internal

{- |
   Module      :  Control.Monad.ST.Trans
   Copyright   :  Josef Svenningsson 2008-2010
                  (c) The University of Glasgow, 1994-2000
   License     :  BSD
 
   Maintainer  :  josef.svenningsson@gmail.com
   Stability   :  experimental
   Portability :  non-portable (GHC Extensions)

   This module provides the implementation of the 'STT' type for those
   occasions where it's needed in order to implement new liftings through
   operations in other monads.

   Warning! This monad transformer should not be used with monads that
   can contain multiple answers, like the list monad. The reason is that 
   the will be duplicated across the different answers and this cause
   Bad Things to happen (such as loss of referential transparency). Safe 
   monads include the monads State, Reader, Writer, Maybe and 
   combinations of their corresponding monad transformers.
-}

-- | 'STT' is the monad transformer providing polymorphic updateable references
newtype STT s m a = STT (State# s -> m (STTRet s a))

unSTT :: STT s m a -> (State# s -> m (STTRet s a))
unSTT (STT f) = f

-- | 'STTRet' is needed to encapsulate the unboxed state token that GHC passes
--   around. This type is essentially a pair, but an ordinary pair is not
--   not allowed to contain unboxed types.
data STTRet s a = STTRet (State# s) a

-- | Lifting the `ST` monad into `STT`. The library uses this function
--   extensively to be able to reuse functions from `ST`.
liftST :: Applicative m => ST s a -> STT s m a
liftST (ST f) = STT (\s -> let !(# s', a #) = f s in pure (STTRet s' a))
{-# INLINE liftST #-}

-- All instances have to go in this module because otherwise they
-- would be orphan instances.

instance Monad m => Monad (STT s m) where
  return a = STT $ \st -> return (STTRet st a)
  STT m >>= k = STT $ \st -> 
    do ret <- m st
       case ret of
         STTRet new_st a -> 
             unSTT (k a) new_st

instance MF.MonadFail m => MF.MonadFail (STT s m) where
  fail msg = lift (fail msg)

instance MonadTrans (STT s) where
  lift m = STT $ \st ->
   do a <- m
      return (STTRet st a)
      
liftSTT :: STT s m a -> State# s -> m (STTRet s a)
liftSTT (STT m) s = m s

instance (MonadFix m) => MonadFix (STT s m) where
  mfix k = STT $ \ s -> mdo
    ans@(STTRet _ r) <- liftSTT (k r) s
    return ans

instance Functor (STTRet s) where
  fmap f (STTRet s a) = STTRet s (f a)

instance Functor m => Functor (STT s m) where
  fmap f (STT g) = STT $ \s# -> (fmap . fmap) f (g s#)

instance (Monad m, Functor m) => Applicative (STT s m) where
  pure a = STT $ \s# -> return (STTRet s# a)
  (STT m) <*> (STT n) = STT $ \s1 ->
                        do (STTRet s2 f) <- m s1
                           (STTRet s3 x) <- n s2
                           return (STTRet s3 (f x))

-- Instances of other monad classes

instance MonadError e m => MonadError e (STT s m) where
  throwError e = lift (throwError e)
  catchError (STT m) f = STT $ \st -> catchError (m st) 
                         (\e -> unSTT (f e) st)

instance MonadReader r m => MonadReader r (STT s m) where
  ask = lift ask
  local f (STT m) = STT $ \st -> local f (m st)

instance MonadState s m => MonadState s (STT s' m) where
  get = lift get
  put s = lift (put s)

instance MonadWriter w m => MonadWriter w (STT s m) where
  tell w = lift (tell w)
  listen (STT m)= STT $ \st1 -> do (STTRet st2 a, w) <- listen (m st1)
                                   return (STTRet st2 (a,w))
  pass (STT m) = STT $ \st1 -> pass (do (STTRet st2 (a,f)) <- m st1
                                        return (STTRet st2 a, f))

-- MArray instances

instance (Applicative m, Monad m) => MArray (STArray s) e (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Bool (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Char (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) (Ptr a) (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) (FunPtr a) (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Float (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Double (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) (StablePtr a) (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int8 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int16 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int32 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Int64 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word8 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word16 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word32 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)

instance (Applicative m, Monad m) => MArray (STUArray s) Word64 (STT s m) where
    {-# INLINE getBounds #-}
    getBounds arr = liftST (getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements arr = liftST (getNumElements arr)
    {-# INLINE newArray #-}
    newArray bnds e = liftST (newArray bnds e)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = liftST (newArray_ arrBounds)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ bnds = liftST (unsafeNewArray_ bnds)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i = liftST (unsafeRead arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = liftST (unsafeWrite arr i e)


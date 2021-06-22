{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, ScopedTypeVariables, CPP #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures, FlexibleContexts, LambdaCase #-}

-- Data.MemoTrie
import Control.Applicative
import Control.Arrow (first,(&&&))
import Data.Bits
import Data.Function (fix)
import Data.Function (on)
import Data.Int
import Data.Void (Void)
import Data.Word
import GHC.Generics


-- Control.Newtype.Generics
import Control.Arrow
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Fixed
import Data.Kind (Type)
import Data.Monoid
import Data.Ord
import qualified Data.Semigroup
import Data.Semigroup (Min(..), Max(..), WrappedMonoid(..), Option(..))

solve :: [Int] -> Int -> Int -> Int
solve ps = s where
  s = memo2 s'
  s' a b = case takeWhile (<= b) $ dropWhile (<a) ps of
             [] -> 0
             ps' -> minimum [ b - a + s a (p-1) + s (p+1) b | p <- ps' ]

splits :: [Int] -> [([Int],Int,[Int])]
splits ns0 = go ns0 []
    where go (n:ns) a = (a,n,ns) : go ns (a++[n])
          go _ _ = []

main :: IO ()
main = interact $ unlines . zipWith c [1 :: Int ..] . test . map read . tail . words
  where c i j = "Case #" ++ show i ++ ": " ++ show j

test :: [Int] -> [Int]
test [] = []
test (p:q:ns) = solve qs 1 p : test ns'
    where (qs,ns') = splitAt q ns

-- SUCCESS

------------------------------------------------------------
-- Data.MemoTrie
{-# OPTIONS_GHC -Wall -fenable-rewrite-rules #-}

-- ScopedTypeVariables works around a 6.10 bug.  The forall keyword is
-- supposed to be recognized in a RULES pragma.

----------------------------------------------------------------------
-- |
-- Module      :  Data.MemoTrie
-- Copyright   :  (c) Conal Elliott 2008-2016
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Trie-based memoizer
-- 
-- Adapted from sjanssen's paste: <http://hpaste.org/3839 \"a lazy trie\">,
-- which I think is based on Ralf Hinze's paper "Memo Functions,
-- Polytypically!".
-- 
-- You can automatically derive generic instances. for example: 
-- 
-- @
-- {-# LANGUAGE <https://ocharles.org.uk/blog/posts/2014-12-16-derive-generic.html DeriveGeneric>, TypeOperators, TypeFamilies #-}
-- import Data.MemoTrie
-- import GHC.Generics (Generic) 
-- 
-- data Color = RGB Int Int Int
--            | NamedColor String 
--  deriving ('Generic') 
-- 
-- instance HasTrie Color where
--   newtype (Color :->: b) = ColorTrie { unColorTrie :: 'Reg' Color :->: b } 
--   trie = 'trieGeneric' ColorTrie 
--   untrie = 'untrieGeneric' unColorTrie
--   enumerate = 'enumerateGeneric' unColorTrie
-- @
-- 
-- see @examples/Generic.hs@, which can be run with: 
-- 
-- @
-- cabal configure -fexamples && cabal run generic
-- @ 
-- 
-- 
----------------------------------------------------------------------

-- import Prelude hiding (id,(.))
-- import Control.Category
-- import Control.Arrow

infixr 0 :->:

-- | Mapping from all elements of @a@ to the results of some function
class HasTrie a where
    -- | Representation of trie with domain type @a@
    data (:->:) a :: * -> *
    -- | Create the trie for the entire domain of a function
    trie   :: (a  ->  b) -> (a :->: b)
    -- | Convert a trie to a function, i.e., access a field of the trie
    untrie :: (a :->: b) -> (a  ->  b)
    -- | List the trie elements.  Order of keys (@:: a@) is always the same.
    enumerate :: (a :->: b) -> [(a,b)]

-- | Domain elements of a trie
domain :: HasTrie a => [a]
domain = map fst (enumerate (trie (const oops)))
 where
   oops = error "Data.MemoTrie.domain: range element evaluated."

-- Hm: domain :: [Bool] doesn't produce any output.

instance (HasTrie a, Eq b) => Eq (a :->: b) where
  (==) = (==) `on` (map snd . enumerate)

instance (HasTrie a, Show a, Show b) => Show (a :->: b) where
  show t = "Trie: " ++ show (enumerate t)

{-
trie2 :: (HasTrie a, HasTrie b) =>
         (a -> b -> c) -> (a :->: b :->: c)
-- trie2 h = trie $ \ a -> trie $ \ b -> h a b
-- trie2 h = trie $ \ a -> trie (h a)
trie2 h = trie (trie . h)
-- trie2 h = trie (fmap trie h)
-- trie2 = (fmap.fmap) trie trie


trie3 :: (HasTrie a, HasTrie b, HasTrie c) =>
         (a -> b -> c -> d) -> (a :->: b :->: c :->: d)
trie3 h = trie (trie2 . h)

untrie2 :: (HasTrie a, HasTrie b) =>
          (a :->: b :->: c)-> (a -> b -> c)
untrie2 tt = untrie . untrie tt


untrie3 :: (HasTrie a, HasTrie b, HasTrie c) =>
          (a :->: b :->: c :->: d)-> (a -> b -> c -> d)
untrie3 tt = untrie2 . untrie tt
-}


-- {-# RULES "trie/untrie"   forall t. trie (untrie t) = t #-}

--     warning: [-Winline-rule-shadowing] …
--     Rule "trie/untrie" may never fire
--       because rule "Class op untrie" for ‘untrie’ might fire first
--     Probable fix: add phase [n] or [~n] to the competing rule


-- Don't include the dual rule:
--   "untrie/trie"   forall f. untrie (trie f) = f
-- which would defeat memoization.
--
-- TODO: experiment with rule application.  Maybe re-enable "untrie/trie"
-- but fiddle with phases, so it won't defeat 'memo'.

-- | Trie-based function memoizer
memo :: HasTrie t => (t -> a) -> (t -> a)
memo = untrie . trie

-- | Memoize a binary function, on its first argument and then on its
-- second.  Take care to exploit any partial evaluation.
memo2 :: (HasTrie s,HasTrie t) => (s -> t -> a) -> (s -> t -> a)

-- | Memoize a ternary function on successive arguments.  Take care to
-- exploit any partial evaluation.
memo3 :: (HasTrie r,HasTrie s,HasTrie t) => (r -> s -> t -> a) -> (r -> s -> t -> a)

-- | Lift a memoizer to work with one more argument.
mup :: HasTrie t => (b -> c) -> (t -> b) -> (t -> c)
mup mem f = memo (mem . f)

memo2 = mup memo
memo3 = mup memo2

-- | Memoizing recursion. Use like `fix`.
memoFix :: HasTrie a => ((a -> b) -> (a -> b)) -> (a -> b)
memoFix h = fix (memo . h)


-- | Apply a unary function inside of a trie
inTrie :: (HasTrie a, HasTrie c) =>
          ((a  ->  b) -> (c  ->  d))
       -> ((a :->: b) -> (c :->: d))
inTrie = untrie ~> trie

-- | Apply a binary function inside of a trie
inTrie2 :: (HasTrie a, HasTrie c, HasTrie e) =>
           ((a  ->  b) -> (c  ->  d) -> (e  ->  f))
        -> ((a :->: b) -> (c :->: d) -> (e :->: f))
inTrie2 = untrie ~> inTrie

-- | Apply a ternary function inside of a trie
inTrie3 :: (HasTrie a, HasTrie c, HasTrie e, HasTrie g) =>
           ((a  ->  b) -> (c  ->  d) -> (e  ->  f) -> (g  ->  h))
        -> ((a :->: b) -> (c :->: d) -> (e :->: f) -> (g :->: h))
inTrie3 = untrie ~> inTrie2


---- Instances

instance HasTrie Void where
  -- As suggested by Audun Skaugen
  data Void :->: a = VoidTrie
  trie _ = VoidTrie
  untrie VoidTrie = \ _ -> error "untrie VoidTrie"
                    -- \case  -- needs EmptyCase
  enumerate VoidTrie = []

instance Newtype (Void :->: a) where
  type O (Void :->: a) = ()
  pack () = VoidTrie
  unpack VoidTrie = ()

instance HasTrie () where
  newtype () :->: a = UnitTrie a
  trie f = UnitTrie (f ())
  untrie (UnitTrie a) = \ () -> a
  enumerate (UnitTrie a) = [((),a)]

instance Newtype (() :->: a) where
  type O (() :->: a) = a
  pack a = UnitTrie a
  unpack (UnitTrie a) = a

-- Proofs of inverse properties:

{-
    untrie (trie f)
      == { trie def }
    untrie (UnitTrie (f ()))
      == { untrie def }
    \ () -> (f ())
      == { const-unit }
    f   

    trie (untrie (UnitTrie a))
      == { untrie def }
    trie (\ () -> a)
      == { trie def }
    UnitTrie ((\ () -> a) ())
      == { beta-reduction }
    UnitTrie a

Oops -- the last step of the first direction is bogus when f is non-strict.
Can be fixed by using @const a@ in place of @\ () -> a@, but I can't do
the same for other types, like integers or sums.

All of these proofs have this same bug, unless we restrict ourselves to
memoizing hyper-strict functions.

-}


instance HasTrie Bool where
  data Bool :->: x = BoolTrie x x
  trie f = BoolTrie (f False) (f True)
  untrie (BoolTrie f t) = if' f t
  enumerate (BoolTrie f t) = [(False,f),(True,t)]

instance Newtype (Bool :->: a) where
  type O (Bool :->: a) = (a,a)
  pack (a,a') = BoolTrie a a'
  unpack (BoolTrie a a') = (a,a')

-- | Conditional with boolean last.
-- Spec: @if' (f False) (f True) == f@
if' :: x -> x -> Bool -> x
if' t _ False = t
if' _ e True  = e

{-
    untrie (trie f)
      == { trie def }
    untrie (BoolTrie (f False) (f True))
      == { untrie def }
    if' (f False) (f True)
      == { if' spec }
    f

    trie (untrie (BoolTrie f t))
      == { untrie def }
    trie (if' f t)
      == { trie def }
    BoolTrie (if' f t False) (if' f t True)
      == { if' spec }
    BoolTrie f t
-}

instance HasTrie a => HasTrie (Maybe a) where
  data (:->:) (Maybe a) b = MaybeTrie b (a :->: b)
  trie f = MaybeTrie (f Nothing) (trie (f . Just))
  untrie (MaybeTrie nothing_val a_trie) = maybe nothing_val (untrie a_trie)
  enumerate (MaybeTrie nothing_val a_trie) = (Nothing, nothing_val) : enum' Just a_trie

instance Newtype (Maybe a :->: x) where
  type O (Maybe a :->: x) = (x, a :->: x)
  pack (a,f) = MaybeTrie a f
  unpack (MaybeTrie a f) = (a,f)

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
  data (Either a b) :->: x = EitherTrie (a :->: x) (b :->: x)
  trie f = EitherTrie (trie (f . Left)) (trie (f . Right))
  untrie (EitherTrie s t) = either (untrie s) (untrie t)
  enumerate (EitherTrie s t) = enum' Left s `weave` enum' Right t

instance Newtype (Either a b :->: x) where
  type O (Either a b :->: x) = (a :->: x, b :->: x)
  pack (f,g) = EitherTrie f g
  unpack (EitherTrie f g) = (f,g)

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap.first) f . enumerate

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a:as) `weave` bs = a : (bs `weave` as)

{-
    untrie (trie f)
       == { trie def }
    untrie (EitherTrie (trie (f . Left)) (trie (f . Right)))
       == { untrie def }
    either (untrie (trie (f . Left))) (untrie (trie (f . Right)))
       == { untrie . trie }
    either (f . Left) (f . Right)
       == { either }
    f

    trie (untrie (EitherTrie s t))
       == { untrie def }
    trie (either (untrie s) (untrie t))
       == { trie def }
    EitherTrie (trie (either (untrie s) (untrie t) . Left))
               (trie (either (untrie s) (untrie t) . Right))
       == { either }
    EitherTrie (trie (untrie s)) (trie (untrie t))
       == { trie . untrie }
    EitherTrie s t
-}


instance (HasTrie a, HasTrie b) => HasTrie (a,b) where
  newtype (a,b) :->: x = PairTrie (a :->: (b :->: x))
  trie f = PairTrie (trie (trie . curry f))
  untrie (PairTrie t) = uncurry (untrie .  untrie t)
  enumerate (PairTrie tt) =
    [ ((a,b),x) | (a,t) <- enumerate tt , (b,x) <- enumerate t ]

instance Newtype ((a,b) :->: x) where
  type O ((a,b) :->: x) = a :->: b :->: x
  pack abx = PairTrie abx
  unpack (PairTrie abx) = abx

{-
    untrie (trie f)
      == { trie def }
    untrie (PairTrie (trie (trie . curry f)))
      == { untrie def }
    uncurry (untrie . untrie (trie (trie . curry f)))
      == { untrie . trie }
    uncurry (untrie . trie . curry f)
      == { untrie . untrie }
    uncurry (curry f)
      == { uncurry . curry }
    f

    trie (untrie (PairTrie t))
      == { untrie def }
    trie (uncurry (untrie .  untrie t))
      == { trie def }
    PairTrie (trie (trie . curry (uncurry (untrie .  untrie t))))
      == { curry . uncurry }
    PairTrie (trie (trie . untrie .  untrie t))
      == { trie . untrie }
    PairTrie (trie (untrie t))
      == { trie . untrie }
    PairTrie t
-}

instance (HasTrie a, HasTrie b, HasTrie c) => HasTrie (a,b,c) where
  newtype (a,b,c) :->: x = TripleTrie (((a,b),c) :->: x)
  trie f = TripleTrie (trie (f . trip))
  untrie (TripleTrie t) = untrie t . detrip
  enumerate (TripleTrie t) = enum' trip t

trip :: ((a,b),c) -> (a,b,c)
trip ((a,b),c) = (a,b,c)

detrip :: (a,b,c) -> ((a,b),c)
detrip (a,b,c) = ((a,b),c)


instance HasTrie x => HasTrie [x] where
  newtype [x] :->: a = ListTrie (Either () (x,[x]) :->: a)
  trie f = ListTrie (trie (f . list))
  untrie (ListTrie t) = untrie t . delist
  enumerate (ListTrie t) = enum' list t

list :: Either () (x,[x]) -> [x]
list = either (const []) (uncurry (:))

delist :: [x] -> Either () (x,[x])
delist []     = Left ()
delist (x:xs) = Right (x,xs)

#define WordInstance(Type,TrieType)\
instance HasTrie Type where \
  newtype Type :->: a = TrieType ([Bool] :->: a);\
  trie f = TrieType (trie (f . unbits));\
  untrie (TrieType t) = untrie t . bits;\
  enumerate (TrieType t) = enum' unbits t

WordInstance(Word,WordTrie)
WordInstance(Word8,Word8Trie)
WordInstance(Word16,Word16Trie)
WordInstance(Word32,Word32Trie)
WordInstance(Word64,Word64Trie)

-- instance HasTrie Word where
--   newtype Word :->: a = WordTrie ([Bool] :->: a)
--   trie f = WordTrie (trie (f . unbits))
--   untrie (WordTrie t) = untrie t . bits
--   enumerate (WordTrie t) = enum' unbits t


-- | Extract bits in little-endian order
bits :: (Num t, Bits t) => t -> [Bool]
bits 0 = []
bits x = testBit x 0 : bits (shiftR x 1)

-- | Convert boolean to 0 (False) or 1 (True)
unbit :: Num t => Bool -> t
unbit False = 0
unbit True  = 1

-- | Bit list to value
unbits :: (Num t, Bits t) => [Bool] -> t
unbits [] = 0
unbits (x:xs) = unbit x .|. shiftL (unbits xs) 1

instance HasTrie Char where
  newtype Char :->: a = CharTrie (Int :->: a)
  untrie (CharTrie t) n = untrie t (fromEnum n)
  trie f = CharTrie (trie (f . toEnum))
  enumerate (CharTrie t) = enum' toEnum t

-- Although Int is a Bits instance, we can't use bits directly for
-- memoizing, because the "bits" function gives an infinite result, since
-- shiftR (-1) 1 == -1.  Instead, convert between Int and Word, and use
-- a Word trie.  Any Integral type can be handled similarly.

#define IntInstance(IntType,WordType,TrieType) \
instance HasTrie IntType where \
  newtype IntType :->: a = TrieType (WordType :->: a); \
  untrie (TrieType t) n = untrie t (fromIntegral n); \
  trie f = TrieType (trie (f . fromIntegral)); \
  enumerate (TrieType t) = enum' fromIntegral t

IntInstance(Int,Word,IntTrie)
IntInstance(Int8,Word8,Int8Trie)
IntInstance(Int16,Word16,Int16Trie)
IntInstance(Int32,Word32,Int32Trie)
IntInstance(Int64,Word64,Int64Trie)

-- For unbounded integers, we don't have a corresponding Word type, so
-- extract the sign bit.

instance HasTrie Integer where
  newtype Integer :->: a = IntegerTrie ((Bool,[Bool]) :->: a)
  trie f = IntegerTrie (trie (f . unbitsZ))
  untrie (IntegerTrie t) = untrie t . bitsZ
  enumerate (IntegerTrie t) = enum' unbitsZ t


unbitsZ :: (Num n, Bits n) => (Bool,[Bool]) -> n
unbitsZ (positive,bs) = sig (unbits bs)
 where
   sig | positive  = id
       | otherwise = negate

bitsZ :: (Num n, Ord n, Bits n) => n -> (Bool,[Bool])
bitsZ = (>= 0) &&& (bits . abs)

-- TODO: make these definitions more systematic.


---- Instances

{-

The \"semantic function\" 'untrie' is a morphism over 'Monoid', 'Functor',
'Applicative', 'Monad', 'Category', and 'Arrow', i.e.,

  untrie mempty          == mempty
  untrie (s `mappend` t) == untrie s `mappend` untrie t

  untrie (fmap f t)      == fmap f (untrie t)

  untrie (pure a)        == pure a
  untrie (tf <*> tx)     == untrie tf <*> untrie tx

  untrie (return a)      == return a
  untrie (u >>= k)       == untrie u >>= untrie . k

  untrie id              == id
  untrie (s . t)         == untrie s . untrie t

  untrie (arr f)         == arr f
  untrie (first t)       == first (untrie t)

These morphism properties imply that all of the expected laws hold,
assuming that we interpret equality semantically (or observationally).
For instance,

  untrie (mempty `mappend` a)
    == untrie mempty `mappend` untrie a
    == mempty `mappend` untrie a
    == untrie a

  untrie (fmap f (fmap g a))
    == fmap f (untrie (fmap g a))
    == fmap f (fmap g (untrie a))
    == fmap (f.g) (untrie a)
    == untrie (fmap (f.g) a)

The implementation instances then follow from applying 'trie' to both
sides of each of these morphism laws.

-}

{-
instance (HasTrie a, Monoid b) => Monoid (a :->: b) where
  mempty  = trie mempty
  s `mappend` t = trie (untrie s `mappend` untrie t)

instance HasTrie a => Functor ((:->:) a) where
  fmap f t      = trie (fmap f (untrie t))

instance HasTrie a => Applicative ((:->:) a) where
  pure b        = trie (pure b)
  tf <*> tx     = trie (untrie tf <*> untrie tx)

instance HasTrie a => Monad ((:->:) a) where
  return a      = trie (return a)
  u >>= k       = trie (untrie u >>= untrie . k)

-- instance Category (:->:) where
--   id            = trie id
--   s . t         = trie (untrie s . untrie t)

-- instance Arrow (:->:) where
--   arr f         = trie (arr f)
--   first t       = trie (first (untrie t))
-}

-- Simplify, using inTrie, inTrie2

instance (HasTrie a, Monoid b) => Monoid (a :->: b) where
  mempty  = trie mempty
instance (HasTrie a, Semigroup b) => Semigroup (a :->: b) where
  (<>)    = inTrie2 (<>)

instance HasTrie a => Functor ((:->:) a) where
  fmap f = inTrie (fmap f)

instance HasTrie a => Applicative ((:->:) a) where
  pure b = trie (pure b)
  (<*>)  = inTrie2 (<*>)

instance HasTrie a => Monad ((:->:) a) where
  return a = trie (return a)
  u >>= k  = trie (untrie u >>= untrie . k)

-- | Identity trie
idTrie :: HasTrie a => a :->: a
idTrie = trie id

infixr 9 @.@
-- | Trie composition
(@.@) :: (HasTrie a, HasTrie b) =>
         (b :->: c) -> (a :->: b) -> (a :->: c)
(@.@) = inTrie2 (.)


-- instance Category (:->:) where
--   id  = idTrie
--   (.) = (.:)

-- instance Arrow (:->:) where
--   arr f = trie (arr f)
--   first = inTrie first

{-

Correctness of these instances follows by applying 'untrie' to each side
of each definition and using the property @'untrie' . 'trie' == 'id'@.

The `Category` and `Arrow` instances don't quite work, however, because of
necessary but disallowed `HasTrie` constraints on the domain type.

-}

---- To go elsewhere

-- Matt Hellige's notation for @argument f . result g@.
-- <http://matt.immute.net/content/pointless-fun>

(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
g ~> f = (f .) . (. g)

{-
-- Examples
f1,f1' :: Int -> Int
f1 n = n + n

f1' = memo f1
-}

-- | just like @void@ 
instance HasTrie (V1 x) where
  data (V1 x :->: b) = V1Trie
  trie _ = V1Trie
  untrie V1Trie = \ _ -> error "untrie V1Trie"
                  -- \case  -- needs EmptyCase
  enumerate V1Trie = []

-- | just like @()@ 
instance HasTrie (U1 x) where
  data (U1 x :->: b) = U1Trie b
  trie f = U1Trie (f U1)
  untrie (U1Trie b) = \U1 -> b
  enumerate (U1Trie b) = [(U1, b)]

-- | wraps @Either (f x) (g x)@ 
instance (HasTrie (f x), HasTrie (g x)) => HasTrie ((f :+: g) x) where
  newtype ((f :+: g) x :->: b) = EitherTrie1 (Either (f x) (g x) :->: b)
  trie f = EitherTrie1 (trie (f . liftSum))
  untrie (EitherTrie1 t) = (untrie t) . dropSum
  enumerate (EitherTrie1 t) = enum' liftSum t

-- | wraps @(f x, g x)@ 
instance (HasTrie (f x), HasTrie (g x)) => HasTrie ((f :*: g) x) where
  newtype ((f :*: g) x :->: b) = PairTrie1 ((f x, g x) :->: b)
  trie f = PairTrie1 (trie (f . liftProduct))
  untrie (PairTrie1 t) = (untrie t) . dropProduct
  enumerate (PairTrie1 t) = enum' liftProduct t

-- | wraps @a@ 
instance (HasTrie a) => HasTrie (K1 i a x) where
  data (K1 i a x :->: b) = K1Trie (a :->: b)
  trie f = K1Trie (trie (f . K1))
  untrie (K1Trie t) = \(K1 a) -> (untrie t) a
  enumerate (K1Trie t) = enum' K1 t

-- | wraps @f x@ 
instance (HasTrie (f x)) => HasTrie (M1 i t f x) where
  data (M1 i t f x :->: b) = M1Trie (f x :->: b)
  trie f = M1Trie (trie (f . M1))
  untrie (M1Trie t) = \(M1 a) -> (untrie t) a
  enumerate (M1Trie t) = enum' M1 t

-- | the data type in a __reg__ular form. 
-- "unlifted" generic representation. (i.e. is a unary type constructor). 
type Reg a = Rep a ()

-- | 'Generic'-friendly default for 'trie'
trieGeneric :: (Generic a, HasTrie (Reg a))
            => ((Reg a :->: b) -> (a :->: b))
            -> (a -> b)
            -> (a :->: b)
trieGeneric theConstructor f = theConstructor (trie (f . to))
{-# INLINEABLE trieGeneric #-}

-- | 'Generic'-friendly default for 'untrie'
untrieGeneric :: (Generic a, HasTrie (Reg a))
              => ((a :->: b) -> (Reg a :->: b))
              -> (a :->: b)
              -> (a -> b)
untrieGeneric theDestructor t = \a -> (untrie (theDestructor t)) (from a)
{-# INLINEABLE untrieGeneric #-}

-- | 'Generic'-friendly default for 'enumerate'
enumerateGeneric :: (Generic a, HasTrie (Reg a))
                 => ((a :->: b) -> (Reg a :->: b))
                 -> (a :->: b)
                 -> [(a, b)]
enumerateGeneric theDestructor t = enum' to (theDestructor t)
{-# INLINEABLE enumerateGeneric #-}

dropProduct :: (f :*: g) a -> (f a, g a)
dropProduct (a :*: b) = (a, b)
{-# INLINEABLE dropProduct #-}

liftProduct :: (f a, g a) -> (f :*: g) a
liftProduct (a, b) = (a :*: b)
{-# INLINEABLE liftProduct #-}

dropSum :: (f :+: g) a -> Either (f a) (g a)
dropSum s = case s of
              L1 x -> Left x
              R1 x -> Right x
{-# INLINEABLE dropSum #-}

liftSum :: Either (f a) (g a) -> (f :+: g) a
liftSum = either L1 R1
{-# INLINEABLE liftSum #-}

------------------------------------------------------------
-- Control.Newtype.Generics

{- |
The 'Newtype' typeclass and related functions.
Primarily pulled from Conor McBride's Epigram work. Some examples:

>>> ala Sum foldMap [1,2,3,4]
10

>>> ala Endo foldMap [(+1), (+2), (subtract 1), (*2)] 3
8

>>> under2 Min (<>) 2 1
1

>>> over All not (All False)
All {getAll = True)

This package includes 'Newtype' instances for all the (non-GHC\/foreign)
newtypes in base (as seen in the examples).
However, there are neat things you can do with this with
/any/ newtype and you should definitely define your own 'Newtype'
instances for the power of this library.
For example, see @ala Cont traverse@, with the proper 'Newtype' instance for Cont.
You can easily define new instances for your newtypes with the help of GHC.Generics

 > {-# LANGUAGE DeriveGeneric #-}
 > import GHC.Generics
 >
 > (...)
 > newtype Example = Example Int
 >   deriving (Generic)
 >
 > instance Newtype Example
 >

This avoids the use of Template Haskell (TH) to get new instances.
-}
{-import Generics.Deriving-}

-- | Given a newtype @n@, we will always have the same unwrapped type @o@,
-- meaning we can represent this with a fundep @n -> o@.
--
-- Any instance of this class just needs to let @pack@ equal to the newtype's
-- constructor, and let @unpack@ destruct the newtype with pattern matching.
{-class Newtype n o | n -> o where-}
  {-pack :: o -> n-}
  {-unpack :: n -> o-}


-- Generic Newtype
class GNewtype n where
  type GO n :: Type
  gpack   :: GO n -> n p
  gunpack :: n p  -> GO n

-- We only need one instance, if these generic functions are only to work for
-- newtypes, as these have a fixed form. For example, for a newtype X = Y,
-- Rep X = D1 ... (C1 ... (S1 ... (K1 ... Y)))
instance GNewtype (D1 d (C1 c (S1 s (K1 i a)))) where
  type GO (D1 d (C1 c (S1 s (K1 i a)))) = a
  gpack   x                     = M1 (M1 (M1 (K1 x)))
  gunpack (M1 (M1 (M1 (K1 x)))) = x

-- Original Newtype class, extended with generic defaults (trivial) and deprived
-- of the second type argument (less trivial, as it involves a type family with
-- a default, plus an equality constraint for the related type family in
-- GNewtype). We do get rid of MultiParamTypeClasses and FunctionalDependencies,
-- though.

-- | As long as the type @n@ is an instance of Generic, you can create an instance
-- with just @instance Newtype n@
class Newtype n where
  type O n :: Type
  type O n = GO (Rep n)

  pack   :: O n -> n
  default pack :: (Generic n, GNewtype (Rep n), O n ~ GO (Rep n)) => O n -> n
  pack = to . gpack

  unpack :: n -> O n
  default unpack :: (Generic n, GNewtype (Rep n), O n ~ GO (Rep n)) => n -> O n
  unpack = gunpack . from

-- |
-- This function serves two purposes:
--
-- 1. Giving you the unpack of a newtype without you needing to remember the name.
--
-- 2. Showing that the first parameter is /completely ignored/ on the value level,
--    meaning the only reason you pass in the constructor is to provide type
--    information.  Typeclasses sure are neat.
--
-- >>> op Identity (Identity 3)
-- 3
op :: (Newtype n,o ~ O n ) => (o -> n) -> n -> o
op _ = unpack

-- | The workhorse of the package. Given a "packer" and a \"higher order function\" (/hof/),
-- it handles the packing and unpacking, and just sends you back a regular old
-- function, with the type varying based on the /hof/ you passed.
--
-- The reason for the signature of the /hof/ is due to 'ala' not caring about structure.
-- To illustrate why this is important, consider this alternative implementation of 'under2':
--
-- > under2 :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
-- >        => (o -> n) -> (n -> n -> n') -> (o -> o -> o')
-- > under2' pa f o0 o1 = ala pa (\p -> uncurry f . bimap p p) (o0, o1)
--
-- Being handed the "packer", the /hof/ may apply it in any structure of its choosing –
-- in this case a tuple.
--
-- >>> ala Sum foldMap [1,2,3,4]
-- 10
ala :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
    => (o -> n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof = ala' pa hof id

-- | This is the original function seen in Conor McBride's work.
-- The way it differs from the 'ala' function in this package,
-- is that it provides an extra hook into the \"packer\" passed to the hof.
-- However, this normally ends up being @id@, so 'ala' wraps this function and
-- passes @id@ as the final parameter by default.
-- If you want the convenience of being able to hook right into the hof,
-- you may use this function.
--
-- >>> ala' Sum foldMap length ["hello", "world"]
-- 10
--
-- >>> ala' First foldMap (readMaybe @Int) ["x", "42", "1"]
-- Just 42
ala' :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
     => (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
ala' _ hof f = unpack . hof (pack . f)

-- | A very simple operation involving running the function \'under\' the newtype.
--
-- >>> under Product (stimes 3) 3
-- 27
under :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
      => (o -> n) -> (n -> n') -> (o -> o')
under _ f = unpack . f . pack

-- | The opposite of 'under'. I.e., take a function which works on the
-- underlying types, and switch it to a function that works on the newtypes.
--
-- >>> over All not (All False)
-- All {getAll = True}
over :: (Newtype n,  Newtype n', o' ~ O n', o ~ O n)
     => (o -> n) -> (o -> o') -> (n -> n')
over _ f = pack . f . unpack

-- | Lower a binary function to operate on the underlying values.
--
-- >>> under2 Any (<>) True False
-- True
--
-- @since 0.5.2
under2 :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
       => (o -> n) -> (n -> n -> n') -> (o -> o -> o')
under2 _ f o0 o1 = unpack $ f (pack o0) (pack o1)

-- | The opposite of 'under2'.
--
-- @since 0.5.2
over2 :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
       => (o -> n) -> (o -> o -> o') -> (n -> n -> n')
over2 _ f n0 n1 = pack $ f (unpack n0) (unpack n1)

-- | 'under' lifted into a Functor.
underF :: (Newtype n, Newtype n', o' ~ O n', o ~ O n, Functor f, Functor g)
       => (o -> n) -> (f n -> g n') -> (f o -> g o')
underF _ f = fmap unpack . f . fmap pack

-- | 'over' lifted into a Functor.
overF :: (Newtype n, Newtype n', o' ~ O n', o ~ O n, Functor f, Functor g)
      => (o -> n) -> (f o -> g o') -> (f n -> g n')
overF _ f = fmap pack . f . fmap unpack

-- Instances from Control.Applicative

instance Newtype (WrappedMonad m a) where
  type O (WrappedMonad m a) = m a
  pack = WrapMonad
  unpack (WrapMonad a) = a

instance Newtype (WrappedArrow a b c) where
  type O (WrappedArrow a b c) = a b c
  pack = WrapArrow
  unpack (WrapArrow a) = a

instance Newtype (ZipList a) where
  type O (ZipList a) = [a]
  pack = ZipList
  unpack (ZipList a) = a

-- Instances from Control.Arrow

instance Newtype (Kleisli m a b) where
  type O (Kleisli m a b) = a -> m b
  pack = Kleisli
  unpack (Kleisli a) = a

instance Newtype (ArrowMonad a b) where
  type O (ArrowMonad a b) = a () b
  pack = ArrowMonad
  unpack (ArrowMonad a) = a

-- Instances from Data.Fixed

-- | @since 0.5.1
instance Newtype (Fixed a) where
  type O (Fixed a) = Integer
  pack = MkFixed
  unpack (MkFixed x) = x

-- Instances from Data.Functor.Compose

-- | @since 0.5.1
instance Newtype (Compose f g a) where
  type O (Compose f g a) = f (g a)
  pack = Compose
  unpack (Compose x) = x

-- Instances from Data.Functor.Const

instance Newtype (Const a x) where
  type O (Const a x) = a
  pack = Const
  unpack (Const a) = a

-- Instances from Data.Functor.Identity

-- | @since 0.5.1
instance Newtype (Identity a) where
  type O (Identity a) = a
  pack = Identity
  unpack (Identity a) = a

-- Instances from Data.Monoid

-- | @since 0.5.1
instance Newtype (Dual a) where
  type O (Dual a) = a
  pack = Dual
  unpack (Dual a) = a

instance Newtype (Endo a) where
  type O (Endo a) = (a -> a)
  pack = Endo
  unpack (Endo a) = a

instance Newtype All where
  type O All = Bool
  pack = All
  unpack (All x) = x

instance Newtype Any where
  type O Any = Bool
  pack = Any
  unpack (Any x) = x

instance Newtype (Sum a) where
  type O (Sum a) = a
  pack = Sum
  unpack (Sum a) = a

instance Newtype (Product a) where
  type O (Product a) = a
  pack = Product
  unpack (Product a) = a

instance Newtype (First a) where
  type O (First a) = Maybe a
  pack = First
  unpack (First a) = a

instance Newtype (Last a) where
  type O (Last a) = Maybe a
  pack = Last
  unpack (Last a) = a

-- | @since 0.5.1
instance Newtype (Alt f a) where
  type O (Alt f a) = f a
  pack = Alt
  unpack (Alt x) = x

-- | @since 0.5.4
-- instance Newtype (Ap f a) where
--   type O (Ap f a) = f a
--   pack = Ap
--   unpack = getAp

-- Instances from Data.Ord

-- | @since 0.5.1
instance Newtype (Down a) where
  type O (Down a) = a
  pack = Down
  unpack (Down a) = a


-- Instances from Data.Semigroup

-- | @since 0.5.1
instance Newtype (Min a) where
  type O (Min a) = a
  pack = Min
  unpack (Min a) = a

-- | @since 0.5.1
instance Newtype (Max a) where
  type O (Max a) = a
  pack = Max
  unpack (Max a) = a

-- | @since 0.5.1
instance Newtype (Data.Semigroup.First a) where
  type O (Data.Semigroup.First a) = a
  pack = Data.Semigroup.First
  unpack (Data.Semigroup.First a) = a

-- | @since 0.5.1
instance Newtype (Data.Semigroup.Last a) where
  type O (Data.Semigroup.Last a) = a
  pack = Data.Semigroup.Last
  unpack (Data.Semigroup.Last a) = a

-- | @since 0.5.1
instance Newtype (WrappedMonoid m) where
  type O (WrappedMonoid m) = m
  pack = WrapMonoid
  unpack (WrapMonoid m) = m

-- | @since 0.5.1
instance Newtype (Option a) where
  type O (Option a) = Maybe a
  pack = Option
  unpack (Option x) = x

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances
  , MultiParamTypeClasses, TypeFamilies
  #-}

-- Data.Heap
import Control.Exception
import Data.Data
import Data.Ord
import Text.Read

-- Data.MemoCombinators
import Prelude hiding (either, lookup, maybe)
import Data.Bits
import qualified Data.Array as Array
import Data.Char (ord,chr)

-- Data.IntTrie
import Control.Applicative
import Data.Bifunctor (first)
import Data.Function (fix)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))

import qualified Data.Set as S
import Data.Array hiding (range)
import Control.Arrow ((&&&),(>>>))
import Control.Monad
import Data.Maybe
import Data.List hiding (lookup)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [r,c,f] <- map read . words <$> getLine
    cv <- cave r c <$> replicateM r getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show' (solveDP2 f cv)

cave :: Int -> Int -> [String] -> Array (Int,Int) Bool
cave r c xs = listArray ((1,1),(r,c)) . map f $ concat xs
    where f '#' = True
          f '.' = False
          f _ = error "Invalid input"

type Cave = Array (Int,Int) Bool
type State = (Int,Int,Int,Cave)
type Node = (Int,State)

solveAstar :: Int -> Cave -> Maybe Int
solveAstar f cv = astar goal (nextStates f) (hSingleton (0,(1,1,0,cv))) S.empty

goal :: State -> Bool
goal (i,_,_,cv) = i == height cv

astar :: (State -> Bool) -> (State -> [Node]) -> MinHeap Node -> S.Set State -> Maybe Int
astar g next open close | isNothing n = Nothing
                           | g s = Just d
                           | S.member s close = astar g next open' close
                           | otherwise = astar g next open'' close'
    where n = hView open
          ((_,s@(_,_,d,_)), open') = fromJust n
          children = next s
          open'' = foldl' (flip hInsert) open' children
          close' = S.insert s close

nextStates :: Int -> State -> [Node]
nextStates f s = map makeNode $ 
                 catMaybes [
                   goLeft f s,
                   goRight f s,
                   digLeft s,
                   digRight s
                 ]

goLeft,goRight :: Int -> State -> Maybe State
goLeft f (i,j,d,cv) = isClear cv (i,j-1) >> fallMove f cv (i,j-1,d)
goRight f (i,j,d,cv) = isClear cv (i,j+1) >> fallMove f cv (i,j+1,d)

isClear,isNotClear :: Alternative m => Cave -> (Int,Int) -> m ()
isClear cv = guard . isClear' cv
isNotClear cv = guard . not . isClear' cv

isClear' :: Cave -> (Int,Int) -> Bool
isClear' cv p = inRange (bounds cv) p && False == cv!p

digLeft,digRight :: State -> Maybe State
digLeft (i,j,d,cv) = isClear cv (i,j-1) >> isNotClear cv (i+1,j-1) >> return (i,j,d+1,cv//[((i+1,j-1),False)])
digRight (i,j,d,cv) = isClear cv (i,j+1) >> isNotClear cv (i+1,j+1) >> return (i,j,d+1,cv//[((i+1,j+1),False)])

fallMove :: Int -> Cave -> (Int,Int,Int) -> Maybe State
fallMove (-1) _ _ = Nothing
fallMove f cv (i,j,d) = if isClear' cv (i+1,j)
                        then fallMove (f-1) cv (i+1,j,d)
                        else return (i,j,d,cv)

makeNode :: State -> Node
makeNode s0@(i,_,d,cv) = (d+maxDig, s0)
    where
      rf = [ length . filter id $ [ cv!(r,j) | j <- [1..cols] ]
             | r <- [i+1..rows] ]
      rf' = groupBy streak rf
      streak a b = a==cols && b==cols
      streakWeight s = (s * (s+1)) `div` 2
      (rows, cols) = snd (bounds cv)
      maxDig = sum $ map (streakWeight . length) rf'

height :: Cave -> Int
height cv = fst . snd $ bounds cv

show' :: Show a => Maybe a -> String
show' Nothing = "No"
show' (Just j) = "Yes " ++ show j

solveDP :: Int -> Cave -> Maybe Int
solveDP f cv = s0 (1,1) [] []
    where
      s0 :: (Int,Int) -> [Int] -> [Int] -> Maybe Int
      s0 = memo3 (pair integral integral) (list integral) (list integral) (solveDP' s0)
      solveDP' :: ((Int,Int) -> [Int] -> [Int] -> Maybe Int) ->
                  (Int,Int) -> [Int] -> [Int] -> Maybe Int
      solveDP' _ (i,_) _ _ | i == (fst . snd $ bounds cv) = Just 0
      solveDP' s (i,j) h1 h2 = if not $ null directs
                               then Just $ minimum directs
                               else if not $ null indirects
                                    then Just $ minimum indirects
                                    else Nothing
          where directs = catMaybes $ map fall' range
                range = [j-left..j+right]
                left = length $ takeWhile (not . (clear (i+1) h2) . succ) $ 
                       takeWhile (clear i h1) $ map (j-) [1..]
                right = length $ takeWhile (not . (clear (i+1) h2) . pred) $
                        takeWhile (clear i h1) $ map (j+) [1..]
                clear i' h j' = j `elem` h || isClear' cv (i',j')
                fall' j' = do
                  guard $ clear (i+1) h2 j'
                  (i',j'',_,_) <- fallMove (f-1) cv (i+1,j',undefined)
                  guard $ i' > i
                  let h' = if i' == i+1 then h2 else []
                  s (i',j'') h' []
                indirects = map succ . catMaybes $ concatMap dig [left'..right']
                left' = head $ filter (not . clear (i+1) h2) range
                right' = last $ filter (not . clear (i+1) h2) range
                dig j' = [ guard (j' < right') >> s (i, max j (j'+1)) h1 h',
                           guard (j' > left') >> s (i, min j (j'-1)) h1 h']
                    where h' = insert j' h2

min' :: Ord a => Maybe a -> Maybe a -> Maybe a
min' Nothing b = b
min' a Nothing = a
min' (Just a) (Just b) = Just $ min a b

minimum' :: Ord a => [Maybe a] -> Maybe a
minimum' = foldl' min' Nothing

solveDP2 :: Int -> Cave -> Maybe Int
solveDP2 f0 cv = fall f0 1 1 1 1
    where 
      fall (-1) _ _ _ _ = Nothing
      fall f i j h1 h2 | h2 < h1 = fall f i j h2 h1
      fall f i j h1 h2 = if full i' j
                         then minimum' [
                                s i jmin (1) l,
                                s i jmax (-1) l,
                                walkLeft,
                                walkRight
                              ]
                         else fall (f-1) i' j j j
          where jmin = last $ takeWhile walkable $ map (j-) [0..j-1]
                jmin' = pred jmin
                jmax = last $ takeWhile walkable $ map (j+) [0..cols-j]
                jmax' = succ jmax
                walkable = clear &&& full i' >>> uncurry (&&)
                i' = succ i
                l = jmax - jmin
                full i'' j' = i' > rows || cv!(i'',j')
                clear j' = (h1 <= j' && j' <= h2) || not (full i j')
                walkLeft = guard (jmin'>=1 && clear jmin') >>
                           fall (f0-1) i' jmin' jmin' jmin'
                walkRight = guard (jmax' <= cols && clear jmax') >>
                            fall (f0-1) i' jmax' jmax' jmax'
      (rows,cols) = snd $ bounds cv
      s = memo4 (arrayRange (1,50)) (arrayRange (1,50)) dir (arrayRange (0,49)) s'
      s' i _ _   _ | i == rows = Just 0
      s' i j dr l = minimum' $
            [ fall (f0-1) i' (j+dr*(c-1)) (j+dr*(c-d)) (j+dr*(c-1)) >>=
              return . (+d)
              | c <- [1..l], d <- [1..c] ]
          where i' = succ i

memo4 :: Memo a -> Memo b -> Memo c -> Memo d -> (a -> b -> c -> d -> r) -> a -> b -> c -> d -> r
memo4 a b c d = a . (memo3 b c d .)

boolToDir :: Bool -> Int
boolToDir True = 1
boolToDir False = -1

dirToBool :: Int -> Bool
dirToBool = (>0)

dir :: (Int -> r) -> Int -> r
dir = wrap boolToDir dirToBool bool

------------------------------------------------------------
-- Data.MemoCombinators

------------------------------------------------
-- |
-- Module    : Data.MemoCombinators
-- Copyright : (c) Luke Palmer 2008-2010
-- License   : BSD3
--
-- Maintainer : Luke Palmer <lrpalmer@gmail.com>
-- Stability  : experimental
--
-- This module provides combinators for building memo tables
-- over various data types, so that the type of table can
-- be customized depending on the application.
--
-- This module is designed to be imported /qualified/, eg.
--
-- > import qualified Data.MemoCombinators as Memo
--
-- Usage is straightforward: apply an object of type @Memo a@
-- to a function of type @a -> b@, and get a memoized function
-- of type @a -> b@.  For example:
--
-- > fib = Memo.integral fib'
-- >    where
-- >    fib' 0 = 0
-- >    fib' 1 = 1
-- >    fib' x = fib (x-1) + fib (x-2)
--------------------------------------------
-- | The type of a memo table for functions of a.
type Memo a = forall r. (a -> r) -> (a -> r)

-- | Given a memoizer for a and an isomorphism between a and b, build
-- a memoizer for b.
wrap :: (a -> b) -> (b -> a) -> Memo a -> Memo b
wrap i j m f = m (f . i) . j

-- | Memoize a two argument function (just apply the table directly for
-- single argument functions).
memo2 :: Memo a -> Memo b -> (a -> b -> r) -> (a -> b -> r)
memo2 a b = a . (b .)

-- | Memoize a three argument function.
memo3 :: Memo a -> Memo b -> Memo c -> (a -> b -> c -> r) -> (a -> b -> c -> r)
memo3 a b c = a . (memo2 b c .)

-- | Memoize the second argument of a function.
memoSecond :: Memo b -> (a -> b -> r) -> (a -> b -> r)
memoSecond b = (b .)

-- | Memoize the third argument of a function.
memoThird :: Memo c -> (a -> b -> c -> r) -> (a -> b -> c -> r)
memoThird c = (memoSecond c .)

bool :: Memo Bool
bool g = cond (g True) (g False)
    where
    cond t _ True  = t
    cond _ f False = f

list :: Memo a -> Memo [a]
list m f = table (f []) (m (\x -> list m (f . (x:))))
    where
    table nil _ [] = nil
    table _ itCons (x:xs) = itCons x xs

char :: Memo Char
char = wrap chr ord integral

-- | Build a table which memoizes all lists of less than the given length.
boundedList :: Int -> Memo a -> Memo [a]
boundedList 0 _ f = f
boundedList n m f = table (f []) (m (\x -> boundedList (n-1) m (f . (x:))))
    where
    table nil _ [] = nil
    table _ cons (x:xs) = cons x xs

either :: Memo a -> Memo b -> Memo (Either a b)
either m m' f = table (m (f . Left)) (m' (f . Right))
    where
    table l _ (Left x) = l x
    table _ r (Right x) = r x

mcMaybe :: Memo a -> Memo (Maybe a)
mcMaybe m f = table (f Nothing) (m (f . Just))
    where
    table n _ Nothing = n
    table _ j (Just x) = j x

unit :: Memo ()
unit f = let m = f () in \() -> m

pair :: Memo a -> Memo b -> Memo (a,b)
pair m m' f = uncurry (m (\x -> m' (\y -> f (x,y))))

-- | Memoize an enum type.
enum :: (Enum a) => Memo a
enum = wrap toEnum fromEnum integral

-- | Memoize an integral type.
integral :: (Integral a) => Memo a
integral = wrap fromInteger toInteger bits

-- | Memoize an ordered type with a bits instance.
bits :: (Num a, Ord a, Bits a) => Memo a
bits f = itApply (fmap f itIdentity)

-- | @switch p a b@ uses the memo table a whenever p gives
-- true and the memo table b whenever p gives false.
switch :: (a -> Bool) -> Memo a -> Memo a -> Memo a
switch p m m' f = table (m f) (m' f)
    where
    table t _ x | p x       = t x
                | otherwise = f x

-- | The type of builders for ranged tables; takes a lower bound and an upper
-- bound, and returns a memo table for that range.
type RangeMemo a = (a,a) -> Memo a

-- | Build a memo table for a range using a flat array.  If items are
-- given outside the range, don't memoize.
arrayRange :: (Array.Ix a) => RangeMemo a
arrayRange rng = switch (Array.inRange rng) (unsafeArrayRange rng) id

-- | Build a memo table for a range using a flat array.  If items are
-- given outside the range, behavior is undefined.
unsafeArrayRange :: (Array.Ix a) => RangeMemo a
unsafeArrayRange rng f = (Array.listArray rng (map f (Array.range rng)) Array.!)


-- | Given a list of ranges, (lazily) build a memo table for each one
-- and combine them using linear search.
chunks :: (Array.Ix a) => RangeMemo a -> [(a,a)] -> Memo a
chunks rmemo cs0 f = lookup (cs0 `zip` map (\rng -> rmemo rng f) cs0)
    where
    lookup [] _ = error "Element non in table"
    lookup ((r,c):cs) x | Array.inRange r x = c x
                        | otherwise = lookup cs x

------------------------------------------------------------
-- Data.IntTrie

-------------------------------------------------------------
-- |
-- Module        : Data.IntTrie
-- Copyright     : (c) Luke Palmer 2010
-- License       : BSD3
--
-- Maintainer    : Luke Palmer <lrpalmer@gmail.com>
-- Stability     : experimental
-- Portability   : Haskell 2010
--
-- Provides a minimal infinite, lazy trie for integral types.
-- It intentionally leaves out ideas such as delete and
-- emptiness so that it can be used lazily, eg. as the target
-- of an infinite foldr.  Essentially its purpose is to be an
-- efficient implementation of a function from integral type,
-- given point-at-a-time modifications.
-------------------------------------------------------------

-- | A trie from integers to values of type a. 
-- 
-- Semantics: [[IntTrie a]] = Integer -> a
data IntTrie a = IntTrie (BitTrie a) a (BitTrie a)  -- negative, 0, positive

data BitTrie a = BitTrie a (BitTrie a) (BitTrie a)

instance Functor BitTrie where
    fmap f ~(BitTrie x l r) = BitTrie (f x) (fmap f l) (fmap f r)

instance Applicative BitTrie where
    pure x = fix (\g -> BitTrie x g g)
    ~(BitTrie f fl fr) <*> ~(BitTrie x xl xr) = BitTrie (f x) (fl <*> xl) (fr <*> xr)

instance Semigroup a => Semigroup (BitTrie a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (BitTrie a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance Functor IntTrie where
    fmap f ~(IntTrie neg z pos) = IntTrie (fmap f neg) (f z) (fmap f pos)

instance Applicative IntTrie where
    pure x = IntTrie (pure x) x (pure x)
    IntTrie fneg fz fpos <*> IntTrie xneg xz xpos = 
        IntTrie (fneg <*> xneg) (fz xz) (fpos <*> xpos)

instance Semigroup a => Semigroup (IntTrie a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (IntTrie a) where
    mempty = pure mempty
    mappend = liftA2 mappend

-- | Apply the trie to an argument.  This is the semantic map.
itApply :: (Ord b, Num b, Bits b) => IntTrie a -> b -> a
itApply (IntTrie neg z pos) x =
    case compare x 0 of
        LT -> itApplyPositive neg (-x)
        EQ -> z
        GT -> itApplyPositive pos x

itApplyPositive :: (Num b, Bits b) => BitTrie a -> b -> a
itApplyPositive (BitTrie one e o) x
    | x == 1 = one
    | testBit x 0 = itApplyPositive o (x `shiftR` 1)
    | otherwise   = itApplyPositive e (x `shiftR` 1)

-- | The identity trie.  
--
-- > apply identity = id
itIdentity :: (Num a, Bits a) => IntTrie a
itIdentity = IntTrie (fmap negate itIdentityPositive) 0 itIdentityPositive

itIdentityPositive :: (Num a, Bits a) => BitTrie a
itIdentityPositive = go
    where
    go = BitTrie 1 (fmap (`shiftL` 1) go) (fmap (\n -> (n `shiftL` 1) .|. 1) go)

-- | Modify the function at one point
--
-- > apply (modify x f t) i | i == x = f (apply t i)
-- >                        | otherwise = apply t i
itModify :: (Ord b, Num b, Bits b) => b -> (a -> a) -> IntTrie a -> IntTrie a
itModify x f ~(IntTrie neg z pos) =
    case compare x 0 of
        LT -> IntTrie (itModifyPositive (-x) f neg) z pos
        EQ -> IntTrie neg (f z) pos
        GT -> IntTrie neg z (itModifyPositive x f pos)

itModifyPositive :: (Num b, Bits b) => b -> (a -> a) -> BitTrie a -> BitTrie a
itModifyPositive x f ~(BitTrie one e o)
    | x == 1      = BitTrie (f one) e o
    | testBit x 0 = BitTrie one e (itModifyPositive (x `shiftR` 1) f o)
    | otherwise   = BitTrie one (itModifyPositive (x `shiftR` 1) f e) o


-- | Modify the function at one point (strict version)
itModify' :: (Ord b, Num b, Bits b) => b -> (a -> a) -> IntTrie a -> IntTrie a
itModify' x f (IntTrie neg z pos) =
    case compare x 0 of
        LT -> (IntTrie $! itModifyPositive' (-x) f neg) z pos
        EQ -> (IntTrie neg $! f z) pos
        GT -> IntTrie neg z $! itModifyPositive' x f pos

itModifyPositive' :: (Num b, Bits b) => b -> (a -> a) -> BitTrie a -> BitTrie a
itModifyPositive' x f (BitTrie one e o)
    | x == 1      = (BitTrie $! f one) e o
    | testBit x 0 = BitTrie one e $! itModifyPositive' (x `shiftR` 1) f o
    | otherwise   = (BitTrie one $! itModifyPositive' (x `shiftR` 1) f e) o


-- | Overwrite the function at one point
--
-- > overwrite i x = modify i (const x)
itOverwrite :: (Ord b, Num b, Bits b) => b -> a -> IntTrie a -> IntTrie a
itOverwrite i x = itModify i (const x)


-- | Negate the domain of the function
--
-- > apply (mirror t) i = apply t (-i)
-- > mirror . mirror = id
itMirror :: IntTrie a -> IntTrie a
itMirror ~(IntTrie neg z pos) = IntTrie pos z neg


-- | Modify the function at a (potentially infinite) list of points in ascending order
--
-- > modifyAscList [(i0, f0)..(iN, fN)] = modify i0 f0 . ... . modify iN fN
itModifyAscList :: (Ord b, Num b, Bits b) => [(b, a -> a)] -> IntTrie a -> IntTrie a
itModifyAscList ifs ~t@(IntTrie neg z pos) =
    case break ((>= 0) . fst) ifs of
        ([],   [])          -> t
        (nifs, (0, f):pifs) -> IntTrie (modifyAscListNegative nifs neg) (f z)
                                       (itModifyAscListPositive pifs pos)
        (nifs, pifs)        -> IntTrie (modifyAscListNegative nifs neg) z
                                       (itModifyAscListPositive pifs pos)
    where modifyAscListNegative = itModifyAscListPositive . map (first negate) . reverse

-- | Modify the function at a (potentially infinite) list of points in descending order
itModifyDescList :: (Ord b, Num b, Bits b) => [(b, a -> a)] -> IntTrie a -> IntTrie a
itModifyDescList ifs = itMirror . itModifyAscList (map (first negate) ifs) . itMirror

itModifyAscListPositive :: (Ord b, Num b, Bits b) => [(b, a -> a)] -> BitTrie a -> BitTrie a
itModifyAscListPositive [] t = t
itModifyAscListPositive ((0, _):_) _ =
    error "modifyAscList: expected strictly monotonic indices"
itModifyAscListPositive ifs@((i, f0):_) ~(BitTrie one e o) = BitTrie one' e' o' where
    (one', ifs')      = if i == 1 then (f0 one, tail ifs) else (one, ifs)
    e'             = itModifyAscListPositive ifsEven e
    o'              = itModifyAscListPositive ifsOdd  o
    (ifsOdd, ifsEven) = both (map $ first (`shiftR` 1)) $ itPartitionIndices ifs'
    both f (x, y)     = (f x, f y)

-- Like `partition (flip testBit 0 . fst)`, except that this version addresses the
-- problem of infinite lists of only odd or only even indices by injecting an `id`
-- into the other result list wherever there are two evens or two odds in a row.
-- This allows `modifyAscListPositive` to return a value as soon as the next index is
-- higher than the current location in the trie instead of scanning for the end of
-- the list, which for infinite lists may never be reached.
itPartitionIndices :: (Num b, Bits b) => [(b, a -> a)] -> ([(b, a -> a)], [(b, a -> a)])
itPartitionIndices []           = ([], [])
itPartitionIndices [x]          = if testBit (fst x) 0 then ([x], []) else ([], [x])
itPartitionIndices (x:xs@(y:_)) = case testBit (fst x) 0 of
    False -> (if testBit (fst y) 0 then o else pad:o, x:e)
    True  -> (x:o, if testBit (fst y) 0 then pad:e else e)
    where ~(o, e) = itPartitionIndices xs
          pad = (fst y - 1, id)

------------------------------------------------------------
-- Data.Heap
-- | A flexible implementation of min-, max-, min-priority, max-priority and
-- custom-priority heaps based on the leftist-heaps from Chris Okasaki's book
-- \"Purely Functional Data Structures\", Cambridge University Press, 1998,
-- chapter 3.1.
--
-- There are different flavours of 'Heap's, each of them following a different
-- strategy when ordering its elements:
--
--  * Choose 'MinHeap' or 'MaxHeap' if you need a simple minimum or maximum heap
--    (which always keeps the minimum/maximum element at the head of the 'Heap').
--
--  * If you wish to manually annotate a value with a priority, e. g. an @IO ()@
--    action with an 'Int' use 'MinPrioHeap' or 'MaxPrioHeap'. They manage
--    @(prio, val)@ tuples so that only the priority (and not the value)
--    influences the order of elements.
--
--  * If you still need something different, define a custom order for the heap
--    elements by implementing an instance of 'HeapItem' and let the maintainer
--    know what's missing.
--
-- All sorts of heaps mentioned above ('MinHeap', 'MaxHeap', 'MinPrioHeap' and
-- 'MaxPrioHeap') are built on the same underlying type: @'HeapT' prio val@. It is
-- a simple minimum priority heap. The trick is, that you never insert @(prio,
-- val)@ pairs directly: You only insert an \"external representation\", usually
-- called @item@, and an appropriate 'HeapItem' instance is used to 'split' the
-- @item@ to a @(prio, val)@ pair. For details refer to the documentation of
-- 'HeapItem'.

-- | /O(1)/. Is the 'HeapT' empty?
hNull :: HeapT prio val -> Bool
hNull = hiIsEmpty

-- | /O(1)/. Create a singleton 'HeapT'.
hSingleton :: (HeapItem pol item) => item -> Heap pol item
hSingleton = (uncurry hiSingleton) . split

-- | /O(log n)/. Insert a single item into the 'HeapT'.
hInsert :: (HeapItem pol item) => item -> Heap pol item -> Heap pol item
hInsert = hiUnion . hSingleton

-- | /O(1)/ for the head, /O(log n)/ for the tail. Find the item with minimal
-- associated priority and remove it from the 'Heap' (i. e. find head and tail
-- of the heap) if it is not empty. Otherwise, 'Nothing' is returned.
hView :: (HeapItem pol item) => Heap pol item -> Maybe (item, Heap pol item)
hView = fmap (\(p, v, h) -> (merge (p, v), h)) . hiView

-- | /O(1)/. Find the item with minimal associated priority on the 'Heap' (i. e.
-- its head) if it is not empty. Otherwise, 'Nothing' is returned.
hViewHead :: (HeapItem pol item) => Heap pol item -> Maybe item
hViewHead = fmap fst . hView

-- | /O(log n)/. Remove the item with minimal associated priority and from the
-- 'Heap' (i. e. its tail) if it is not empty. Otherwise, 'Nothing' is returned.
hViewTail :: (HeapItem pol item) => Heap pol item -> Maybe (Heap pol item)
hViewTail = fmap snd . hView

-- | Remove all items from a 'HeapT' not fulfilling a predicate.
hFilter :: (HeapItem pol item) => (item -> Bool) -> Heap pol item -> Heap pol item
hFilter p = fst . (hPartition p)

-- | Partition the 'Heap' into two. @'partition' p h = (h1, h2)@: All items in
-- @h1@ fulfil the predicate @p@, those in @h2@ don't. @'union' h1 h2 = h@.
hPartition :: (HeapItem pol item)
    => (item -> Bool) -> Heap pol item -> (Heap pol item, Heap pol item)
hPartition = hiPartition . splitF

-- | Take the first @n@ items from the 'Heap'.
hTake :: (HeapItem pol item) => Int -> Heap pol item -> [item]
hTake n = fst . hSplitAt n

-- | Remove first @n@ items from the 'Heap'.
hDrop :: (HeapItem pol item) => Int -> Heap pol item -> Heap pol item
hDrop n = snd . hSplitAt n

-- | @'splitAt' n h@: Return a list of the first @n@ items of @h@ and @h@, with
-- those elements removed.
hSplitAt :: (HeapItem pol item) => Int -> Heap pol item -> ([item], Heap pol item)
hSplitAt n heap = let (xs, heap') = hiSplitAt n heap in (fmap merge xs, heap')

-- | @'takeWhile' p h@: List the longest prefix of items in @h@ that satisfy @p@.
hTakeWhile :: (HeapItem pol item) => (item -> Bool) -> Heap pol item -> [item]
hTakeWhile p = fst . (hSpan p)

-- | @'dropWhile' p h@: Remove the longest prefix of items in @h@ that satisfy
-- @p@.
hDropWhile :: (HeapItem pol item)
    => (item -> Bool) -> Heap pol item -> Heap pol item
hDropWhile p = snd . (hSpan p)

-- | @'span' p h@: Return the longest prefix of items in @h@ that satisfy @p@ and
-- @h@, with those elements removed.
hSpan :: (HeapItem pol item)
    => (item -> Bool) -> Heap pol item -> ([item], Heap pol item)
hSpan p heap = let (xs, heap') = hiSpan (splitF p) heap in (fmap merge xs, heap')

-- | @'break' p h@: The longest prefix of items in @h@ that do /not/ satisfy @p@
-- and @h@, with those elements removed.
hBreak :: (HeapItem pol item)
    => (item -> Bool) -> Heap pol item -> ([item], Heap pol item)
hBreak p = hSpan (not . p)

-- | /O(n log n)/. Build a 'Heap' from the given items. Assuming you have a
-- sorted list, you probably want to use 'fromDescList' or 'fromAscList', they
-- are faster than this function.
hFromList :: (HeapItem pol item) => [item] -> Heap pol item
hFromList = hiFromList . fmap split

-- | /O(n log n)/. List all items of the 'Heap' in no specific order.
hToList :: (HeapItem pol item) => Heap pol item -> [item]
hToList = fmap merge . hiToList

-- | /O(n)/. Create a 'Heap' from a list providing its items in ascending order
-- of priority (i. e. in the same order they will be removed from the 'Heap').
-- This function is faster than 'fromList' but not as fast as 'fromDescList'.
--
-- /The precondition is not checked/.
hFromAscList :: (HeapItem pol item) => [item] -> Heap pol item
hFromAscList = hFromDescList . reverse

-- | /O(n log n)/. List the items of the 'Heap' in ascending order of priority.
hToAscList :: (HeapItem pol item) => Heap pol item -> [item]
hToAscList = fmap merge . hiToAscList

-- | /O(n)/. Create a 'Heap' from a list providing its items in descending order
-- of priority (i. e. they will be removed inversely from the 'Heap'). Prefer
-- this function over 'fromList' and 'fromAscList', it's faster.
--
-- /The precondition is not checked/.
hFromDescList :: (HeapItem pol item) => [item] -> Heap pol item
hFromDescList = hiFromDescList . fmap split

-- | /O(n log n)/. List the items of the 'Heap' in descending order of priority.
-- Note that this function is not especially efficient (it is implemented in
-- terms of 'reverse' and 'toAscList'), it is provided as a counterpart of the
-- efficient 'fromDescList' function.
hToDescList :: (HeapItem pol item) => Heap pol item -> [item]
hToDescList = reverse . hToAscList

------------------------------------------------------------
-- Data.Heap.Item

-- | This module provides the 'HeapItem' type family along with necessary
-- instance declarations used to translate between inserted items and the
-- priority-value pairs needed by the minimum priority heap of the module
-- "Data.Heap.Internal".

-- | This type alias is an abbreviation for a 'HeapT' which uses the 'HeapItem'
-- instance of @pol item@ to organise its elements.
type Heap pol item = HeapT (Prio pol item) (Val pol item)

-- | A 'Heap' which will always extract the minimum first.
type MinHeap a = Heap MinPolicy a

-- | A 'Heap' which will always extract the maximum first.
type MaxHeap a = Heap MaxPolicy a

-- | A 'Heap' storing priority-value pairs @(prio, val)@. The order of elements
-- is solely determined by the priority @prio@, the value @val@ has no influence.
-- The priority-value pair with minmal priority will always be extracted first.
type MinPrioHeap prio val = Heap FstMinPolicy (prio, val)

-- | A 'Heap' storing priority-value pairs @(prio, val)@. The order of elements
-- is solely determined by the priority @prio@, the value @val@ has no influence.
-- The priority-value pair with maximum priority will always be extracted first.
type MaxPrioHeap prio val = Heap FstMaxPolicy (prio, val)

-- | @'HeapItem' pol item@ is a type class for items that can be stored in a
-- 'HeapT'. A raw @'HeapT' prio val@ only provides a minimum priority heap (i. e.
-- @val@ doesn't influence the ordering of elements and the pair with minimal
-- @prio@ will be extracted first, see 'HeapT' documentation). The job of this
-- class is to translate between arbitrary @item@s and priority-value pairs
-- @('Prio' pol item, 'Val' pol item)@, depending on the policy @pol@ to be used.
-- This way, we are able to use 'HeapT' not only as 'MinPrioHeap', but also as
-- 'MinHeap', 'MaxHeap', 'MaxPrioHeap' or a custom implementation. In short: The
-- job of this class is to deconstruct arbitrary @item@s into a @(prio, val)@
-- pairs that can be handled by a minimum priority 'HeapT'.
--
-- Example: Consider you want to use @'HeapT' prio val@ as a @'MaxHeap' a@. You
-- would have to invert the order of @a@ (e. g. by introducing @newtype InvOrd a
-- = InvOrd a@ along with an apropriate 'Ord' instance for it) and then use a
-- @type 'MaxHeap' a = 'HeapT' (InvOrd a) ()@. You'd also have to translate
-- every @x@ to @(InvOrd x, ())@ before insertion and back after removal in
-- order to retrieve your original type @a@.
--
-- This functionality is provided by the 'HeapItem' class. In the above example,
-- you'd use a 'MaxHeap'. The according instance declaration is of course
-- already provided and looks like this (simplified):
--
-- @data 'MaxPolicy'
--
-- instance ('Ord' a) => 'HeapItem' 'MaxPolicy' a where
--     newtype 'Prio' 'MaxPolicy' a = MaxP a deriving ('Eq')
--     type    'Val'  'MaxPolicy' a = ()
--     'split' x           = (MaxP x, ())
--     'merge' (MaxP x, _) = x
--
-- instance ('Ord' a) => 'Ord' ('Prio' 'MaxPolicy' a) where
--     'compare' (MaxP x) (MaxP y) = 'compare' y x
-- @
--
-- 'MaxPolicy' is a phantom type describing which 'HeapItem' instance is actually
-- meant (e. g. we have to distinguish between 'MinHeap' and 'MaxHeap', which is
-- done via 'MinPolicy' and 'MaxPolicy', respectively) and @MaxP@ inverts the
-- ordering of @a@, so that the maximum will be on top of the 'HeapT'.
--
-- The conversion functions 'split' and 'merge' have to make sure that
--
-- (1) @forall p v. 'split' ('merge' (p, v)) == (p, v)@ ('merge' and 'split'
--     don't remove, add or alter anything)
--
-- (2) @forall p v f. 'fst' ('split' ('merge' (p, f v)) == 'fst' ('split'
--     ('merge' (p, v)))@ (modifying the associated value @v@ doesn't alter the
--      priority @p@)
class (Ord (Prio pol item)) => HeapItem pol item where
    -- | The part of @item@ that determines the order of elements on a 'HeapT'.
    data Prio pol item :: *
    -- | Everything not part of @'Prio' pol item@
    type Val  pol item :: *

    -- | Translate an @item@ into a priority-value pair.
    split :: item -> (Prio pol item, Val pol item)
    -- | Restore the @item@ from a priority-value pair.
    merge :: (Prio pol item, Val pol item) -> item
{-# RULES "split/merge" forall x. split (merge x) = x #-}

-- | Policy type for a 'MinHeap'.
data MinPolicy

instance (Ord a) => HeapItem MinPolicy a where
    newtype Prio MinPolicy a = MinP { unMinP :: a } deriving (Eq, Ord)
    type    Val  MinPolicy a = ()

    split x           = (MinP x, ())
    merge (MinP x, _) = x

instance (Read a) => Read (Prio MinPolicy a) where
    readPrec     = fmap MinP readPrec
    readListPrec = fmap (fmap MinP) readListPrec

instance (Show a) => Show (Prio MinPolicy a) where
    show        = show . unMinP
    showsPrec d = showsPrec d . unMinP
    showList    = showList . (fmap unMinP)

-- | Policy type for a 'MaxHeap'.
data MaxPolicy

instance (Ord a) => HeapItem MaxPolicy a where
    newtype Prio MaxPolicy a = MaxP { unMaxP :: a } deriving (Eq)
    type    Val  MaxPolicy a = ()

    split x           = (MaxP x, ())
    merge (MaxP x, _) = x

instance (Ord a) => Ord (Prio MaxPolicy a) where
    compare (MaxP x) (MaxP y) = compare y x

instance (Read a) => Read (Prio MaxPolicy a) where
    readPrec     = fmap MaxP readPrec
    readListPrec = fmap (fmap MaxP) readListPrec

instance (Show a) => Show (Prio MaxPolicy a) where
    show        = show . unMaxP
    showsPrec d = showsPrec d . unMaxP
    showList    = showList . (fmap unMaxP)

-- | Policy type for a @(prio, val)@ 'MinPrioHeap'.
data FstMinPolicy

instance (Ord prio) => HeapItem FstMinPolicy (prio, val) where
    newtype Prio FstMinPolicy (prio, val) = FMinP { unFMinP :: prio } deriving (Eq, Ord)
    type    Val  FstMinPolicy (prio, val) = val

    split (p,       v) = (FMinP p, v)
    merge (FMinP p, v) = (p,       v)

instance (Read prio) => Read (Prio FstMinPolicy (prio, val)) where
    readPrec     = fmap FMinP readPrec
    readListPrec = fmap (fmap FMinP) readListPrec

instance (Show prio) => Show (Prio FstMinPolicy (prio, val)) where
    show        = show . unFMinP
    showsPrec d = showsPrec d . unFMinP
    showList    = showList . (fmap unFMinP)

-- | Policy type for a @(prio, val)@ 'MaxPrioHeap'.
data FstMaxPolicy

instance (Ord prio) => HeapItem FstMaxPolicy (prio, val) where
    newtype Prio FstMaxPolicy (prio, val) = FMaxP { unFMaxP :: prio } deriving (Eq)
    type    Val  FstMaxPolicy (prio, val) = val

    split (p,       v) = (FMaxP p, v)
    merge (FMaxP p, v) = (p,       v)

instance (Ord prio) => Ord (Prio FstMaxPolicy (prio, val)) where
    compare (FMaxP x) (FMaxP y) = compare y x

instance (Read prio) => Read (Prio FstMaxPolicy (prio, val)) where
    readPrec     = fmap FMaxP readPrec
    readListPrec = fmap (fmap FMaxP) readListPrec

instance (Show prio) => Show (Prio FstMaxPolicy (prio, val)) where
    show        = show . unFMaxP
    showsPrec d = showsPrec d . unFMaxP
    showList    = showList . (fmap unFMaxP)

-- | 'split' a function on @item@s to one on priority-value pairs.
splitF :: (HeapItem pol item) => (item -> a) -> (Prio pol item, Val pol item) -> a
splitF f pv = f (merge pv)
{-# INLINE[1] splitF #-}
{-# RULES "splitF/split" forall f x. splitF f (split x) = f x #-}

------------------------------------------------------------
-- Data.Heap.Internal

{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides a simple leftist-heap implementation based on Chris
-- Okasaki's book \"Purely Functional Data Structures\", Cambridge University
-- Press, 1998, chapter 3.1.
--
-- A @'HeapT' prio val@ associates a priority @prio@ to a value @val@. A
-- priority-value pair with minimum priority will always be the head of the
-- 'HeapT', so this module implements minimum priority heaps. Note that the value
-- associated to the priority has no influence on the ordering of elements, only
-- the priority does.

-- | The basic heap type. It stores priority-value pairs @(prio, val)@ and
-- always keeps the pair with minimal priority on top. The value associated to
-- the priority does not have any influence on the ordering of elements.
data HeapT prio val
    = Empty  -- ^ An empty 'HeapT'.
    | Tree { _rank     :: {-# UNPACK #-} !Int -- ^ Rank of the leftist heap.
           , _size     :: {-# UNPACK #-} !Int -- ^ Number of elements in the heap.
           , _priority :: !prio               -- ^ Priority of the entry.
           , _value    :: val                 -- ^ Value of the entry.
           , _left     :: !(HeapT prio val)   -- ^ Left subtree.
           , _right    :: !(HeapT prio val)   -- ^ Right subtree.
           } -- ^ A tree node of a non-empty 'HeapT'.
    deriving (Typeable)

instance (Read prio, Read val, Ord prio) => Read (HeapT prio val) where
    readPrec     = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        fmap hiFromList readPrec
    readListPrec = readListPrecDefault

instance (Show prio, Show val) => Show (HeapT prio val) where
    showsPrec d heap = showParen (d > 10)
        $ showString "fromList " . (showsPrec 11 (hiToList heap))

instance (Ord prio, Ord val) => Eq (HeapT prio val) where
    heap1 == heap2 = hiSize heap1 == hiSize heap2 && EQ == compare heap1 heap2

instance (Ord prio, Ord val) => Ord (HeapT prio val) where
    compare = comparing hiToPairAscList

instance (Ord prio) => Semigroup (HeapT prio val) where
    (<>) = hiUnion

instance (Ord prio) => Monoid (HeapT prio val) where
    mempty  = hiEmpty
    mappend = hiUnion
    mconcat = hiUnions

instance Functor (HeapT prio) where
    fmap _ Empty = Empty
    fmap f heap  = heap { _value = f (_value heap)
                        , _left  = fmap f (_left heap)
                        , _right = fmap f (_right heap)
                        }

instance (Ord prio) => Foldable (HeapT prio) where
    foldMap f = foldMap f . fmap snd . hiToAscList
    foldr f z = foldl (flip f) z . fmap snd . reverse . hiToAscList
    foldl f z = foldl f z . fmap snd . hiToAscList

-- | /O(1)/. Is the 'HeapT' empty?
hiIsEmpty :: HeapT prio val -> Bool
hiIsEmpty Empty = True
hiIsEmpty _     = False

-- | /O(1)/. Find the rank of a 'HeapT' (the length of its right spine).
rank :: HeapT prio val -> Int
rank Empty = 0
rank heap  = _rank heap

-- | /O(1)/. The total number of elements in the 'HeapT'.
hiSize :: HeapT prio val -> Int
hiSize Empty = 0
hiSize heap  = _size heap

-- | /O(1)/. Construct an empty 'HeapT'.
hiEmpty :: HeapT prio val
hiEmpty = Empty

-- | /O(1)/. Create a singleton 'HeapT'.
hiSingleton :: prio -> val -> HeapT prio val
hiSingleton p v = Tree { _rank     = 1
                     , _size     = 1
                     , _priority = p
                     , _value    = v
                     , _left     = hiEmpty
                     , _right    = hiEmpty
                     }
{-# INLINE hiSingleton #-}

-- | /O(1)/. Insert an priority-value pair into the 'HeapT', whose /priority is
-- less or equal/ to all other priorities on the 'HeapT', i. e. a pair that is a
-- valid head of the 'HeapT'.
--
-- /The precondition is not checked/.
hiUncheckedCons :: (Ord prio) => prio -> val -> HeapT prio val -> HeapT prio val
hiUncheckedCons p v heap = assert (maybe True (\(p', _, _) -> p <= p') (hiView heap))
    Tree { _rank     = 1
         , _size     = 1 + hiSize heap
         , _priority = p
         , _value    = v
         , _left     = heap
         , _right    = hiEmpty
         }
{-# INLINE hiUncheckedCons #-}

-- | /O(log max(n, m))/. Form the union of two 'HeapT's.
hiUnion :: (Ord prio) => HeapT prio val -> HeapT prio val -> HeapT prio val
hiUnion heap  Empty = heap
hiUnion Empty heap  = heap
hiUnion heap1 heap2 = let
    p1 = _priority heap1
    p2 = _priority heap2
    in if p1 < p2
        then hiMakeT p1 (_value heap1) (_left heap1) (hiUnion (_right heap1) heap2)
        else hiMakeT p2 (_value heap2) (_left heap2) (hiUnion (_right heap2) heap1)

-- | Build a 'HeapT' from a priority, a value and two more 'HeapT's. Therefore,
-- the /priority has to be less or equal/ than all priorities in both 'HeapT'
-- parameters.
--
-- /The precondition is not checked/.
hiMakeT :: (Ord prio) => prio -> val -> HeapT prio val -> HeapT prio val -> HeapT prio val
hiMakeT p v a b = let
    ra = rank a
    rb = rank b
    s  = hiSize a + hiSize b + 1
    in assert (checkPrio a && checkPrio b) $ if ra > rb
        then Tree (rb + 1) s p v a b
        else Tree (ra + 1) s p v b a
    where
    checkPrio = maybe True (\(p', _, _) -> p <= p') . hiView
{-# INLINE hiMakeT #-}

-- | Build the union of all given 'HeapT's.
hiUnions :: (Ord prio) => [HeapT prio val] -> HeapT prio val
hiUnions heaps = case tournamentFold' heaps of
    []  -> hiEmpty
    [h] -> h
    hs  -> hiUnions hs
    where
    tournamentFold' :: (Monoid m) => [m] -> [m]
    tournamentFold' (x1:x2:xs) = (: tournamentFold' xs) $! mappend x1 x2
    tournamentFold' xs         = xs

-- | /O(log n)/ for the tail, /O(1)/ for the head. Find the priority-value pair
-- with minimal priority and delete it from the 'HeapT' (i. e. find head and tail
-- of the heap) if it is not empty. Otherwise, 'Nothing' is returned.
hiView :: (Ord prio) => HeapT prio val -> Maybe (prio, val, HeapT prio val)
hiView Empty = Nothing
hiView heap  = Just (_priority heap, _value heap, hiUnion (_left heap) (_right heap))
{-# INLINE hiView #-}

-- | Partition the 'HeapT' into two. @'partition' p h = (h1, h2)@: All
-- priority-value pairs in @h1@ fulfil the predicate @p@, those in @h2@ don't.
-- @'union' h1 h2 = h@.
hiPartition :: (Ord prio) => ((prio, val) -> Bool) -> HeapT prio val
    -> (HeapT prio val, HeapT prio val)
hiPartition _ Empty  = (hiEmpty, hiEmpty)
hiPartition f heap
    | f (p, v)  = (hiMakeT p v l1 r1, hiUnion l2 r2)
    | otherwise = (hiUnion l1 r1, hiMakeT p v l2 r2)
    where
    (p, v)   = (_priority heap, _value heap)
    (l1, l2) = hiPartition f (_left heap)
    (r1, r2) = hiPartition f (_right heap)
{-# INLINE hiPartition #-}

-- | @'splitAt' n h@: A list of the lowest @n@ priority-value pairs of @h@, in
--  ascending order of priority, and @h@, with those elements removed.
hiSplitAt :: (Ord prio) => Int -> HeapT prio val -> ([(prio, val)], HeapT prio val)
hiSplitAt n heap
    | n > 0     = case hiView heap of
        Nothing         -> ([], hiEmpty)
        Just (p, v, hs) -> let (xs, heap') = hiSplitAt (n-1) hs in ((p, v):xs, heap')
    | otherwise = ([], heap)
{-# INLINE hiSplitAt #-}

-- | @'span' p h@: The longest prefix of priority-value pairs of @h@, in
-- ascending order of priority, that satisfy @p@ and @h@, with those elements
-- removed.
hiSpan :: (Ord prio) => ((prio, val) -> Bool) -> HeapT prio val
     -> ([(prio, val)], HeapT prio val)
hiSpan f heap = case hiView heap of
    Nothing         -> ([], hiEmpty)
    Just (p, v, hs) -> let pv = (p, v)
        in if f pv
            then let (xs, heap') = hiSpan f hs in (pv:xs, heap')
            else ([], heap)
{-# INLINE hiSpan #-}

-- | /O(n log n)/. Build a 'HeapT' from the given priority-value pairs.
hiFromList :: (Ord prio) => [(prio, val)] -> HeapT prio val
hiFromList = hiFromDescList . sortBy (flip (comparing fst))
{-# INLINE hiFromList #-}

-- | /O(n log n)/. List all priority-value pairs of the 'HeapT' in no specific
-- order.
hiToList :: HeapT prio val -> [(prio, val)]
hiToList Empty = []
hiToList heap  = let
    left  = _left heap
    right = _right heap
    in
    (_priority heap, _value heap) : if (hiSize right) < (hiSize left)
        then hiToList right ++ hiToList left
        else hiToList left  ++ hiToList right
{-# INLINE hiToList #-}

-- | /O(n)/. Create a 'HeapT' from a list providing its priority-value pairs in
-- descending order of priority.
--
-- /The precondition is not checked/.
hiFromDescList :: (Ord prio) => [(prio, val)] -> HeapT prio val
hiFromDescList = foldl' (\h (p, v) -> hiUncheckedCons p v h) hiEmpty
{-# INLINE hiFromDescList #-}

-- | /O(n log n)/. List the priority-value pairs of the 'HeapT' in ascending
-- order of priority.
hiToAscList :: (Ord prio) => HeapT prio val -> [(prio, val)]
hiToAscList = fst . hiSpan (const True)
{-# INLINE hiToAscList #-}

-- | List the priority-value pairs of the 'HeapT' just like 'toAscList' does,
-- but don't ignore the value @val@ when sorting.
hiToPairAscList :: (Ord prio, Ord val) => HeapT prio val -> [(prio, val)]
hiToPairAscList = concat
    . fmap (sortBy (comparing snd))
    . groupBy (\x y -> fst x == fst y)
    . hiToAscList

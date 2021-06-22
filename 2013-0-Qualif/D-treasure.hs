{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Array    ((!),listArray,accumArray,elems)
import Data.Maybe    (listToMaybe)
import Data.List     (delete,sort,foldl')
import Control.Monad (forM_,replicateM,guard)

import Control.DeepSeq (NFData)
import Data.Data (Typeable)
import Data.Bits
import Foreign (Storable)

readInts :: IO [Int]
readInts = fmap (map read . words) getLine

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [_k,n] <- readInts
    startKeys <- readInts
    rawChests <- replicateM n readInts
    let chestsOpenedBy = accumArray (++) [] (1,200) $
                         zip (map head rawChests) (map return [1..])
        keysInChest = listArray (1,n) $ map (tail . tail) rawChests
        keyOpening  = listArray (1,n) $ map     head      rawChests
        startChests = bsFromList [1..n]
        enoughKeys = all (>= 0) $ elems $ accumArray (+) 0 (1,200) $
                     map (flip (,) 1) (startKeys ++ concat (elems keysInChest))
                  ++ map (flip (,) (-1 :: Int)) (elems keyOpening)
        solve keys lockedChests trail
            | bsNull lockedChests = return (reverse trail)
            | dfs keys lockedChests = fail "broken link"
            | otherwise = cs >>= \c ->
                          solve (keysInChest!c ++ delete (keyOpening!c) keys)
                                (bsDelete c lockedChests) (c : trail)
            where cs = sort $ filter (flip bsMember lockedChests) $
                       concatMap (chestsOpenedBy!) keys
        dfs _ cs | bsNull cs = False
        dfs [] _             = True
        dfs (k:ks) cs        = dfs (ks' ++ ks) (cs `bsDifference` bsFromList cs')
            where cs' = filter (flip bsMember cs) (chestsOpenedBy ! k)
                  ks' = concatMap (keysInChest!) cs'
        result = guard enoughKeys >> solve startKeys startChests []
    putStrLn $ "Case #" ++ show i ++ ": " ++
               maybe "IMPOSSIBLE" (unwords . map show) (listToMaybe result)

-- Eliminate the FasterInteger concept, else Data.BitSet
type BitSet = GSBitSet Integer

-- | /O(1)/. Is the bit set empty?
bsNull :: BitSet a -> Bool
bsNull = gsNull
{-# INLINE bsNull #-}

-- | /O(1)/. Ask whether the item is in the bit set.
bsMember :: Enum a => a -> BitSet a -> Bool
bsMember = gsMember
{-# INLINE bsMember #-}

-- | /O(1)/. Delete an item from the bit set.
bsDelete :: Enum a => a -> BitSet a -> BitSet a
bsDelete = gsDelete
{-# INLINE bsDelete #-}

-- | /O(1)/. Difference of two bit sets.
bsDifference :: BitSet a -> BitSet a -> BitSet a
bsDifference = gsDifference
{-# INLINE bsDifference #-}

-- | /O(n)/. Make a bit set from a list of elements.
bsFromList :: Enum a => [a] -> BitSet a
bsFromList = gsFromList
{-# INLINE bsFromList #-}

------------------------------------------------------------
-- Data.BitSet.Generic

-- | A bit set with unspecified container type.
newtype GSBitSet c a = GSBitSet { getBits :: c }
   deriving (Eq, NFData, Storable, Ord, Typeable)

-- | /O(1)/. Is the bit set empty?
gsNull :: (Eq c, Num c) => GSBitSet c a -> Bool
gsNull = (== 0) . getBits
{-# INLINE gsNull #-}

-- | /O(d)/. Ask whether the item is in the bit set.
gsMember :: (Enum a , Bits c) => a -> GSBitSet c a -> Bool
gsMember x = (`testBit` fromEnum x) . getBits
{-# INLINE gsMember #-}

-- | /O(d)/. Delete an item from the bit set.
gsDelete :: (Enum a, Bits c) => a -> GSBitSet c a -> GSBitSet c a
gsDelete x (GSBitSet bits ) = GSBitSet $ bits `clearBit` fromEnum x
{-# INLINE gsDelete #-}

-- | /O(max(m, n))/. Difference of two bit sets.
gsDifference :: Bits c => GSBitSet c a -> GSBitSet c a -> GSBitSet c a
gsDifference (GSBitSet bits1) (GSBitSet bits2) = GSBitSet $ bits1 .&. complement bits2
{-# INLINE gsDifference #-}

-- | /O(d * n)/. Make a bit set from a list of elements.
gsFromList :: (Enum a, Bits c, Num c) => [a] -> GSBitSet c a
gsFromList = GSBitSet . foldl' (\i x -> i `setBit` fromEnum x) 0

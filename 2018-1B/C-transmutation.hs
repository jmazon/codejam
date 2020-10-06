{-# LANGUAGE FlexibleContexts #-}

import qualified Data.ByteString.Char8 as B

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import Control.Applicative (liftA2)
import Control.Monad (forM)
import Control.Arrow (second)

import Data.Array.Unboxed

-- import qualified Data.IntSet as S
import Data.Word (Word64)
import Data.Bits (testBit,setBit)

import System.Mem

type Cookbook = Array Int (Int,Int)
type Metals = UArray Int Int

-- main = do
--   inputs <- parse <$> getContents
--   answers <- forM inputs $ \i -> do
--     -- performGC
--     let r = uncurry solve i
--     r `seq` return r
--   putStr $ unlines $ zipWith format [1..] answers
    
main = B.interact $ B.unlines . zipWith format [1..] .
                    map (uncurry solve) . parse

format :: Int -> Int -> B.ByteString
format x y = B.pack $ "Case #" ++ show x ++ ": " ++ show y

parse :: B.ByteString -> [(Cookbook,Metals)]
parse = go . map read' . tail . B.words where
  go [] = []
  go (m:xs) = ( listArray (1,m) (parseCookbook cb)
              , listArray (1,m) ms) : go xs'' where
    (cb,xs') = splitAt (2*m) xs
    (ms,xs'') = splitAt m xs'
  parseCookbook (ri1:ri2:xs) = (ri1,ri2) : parseCookbook xs
  parseCookbook [] = []
  read' s = i where Just (i,_) = B.readInt s

-- type Set = S.IntSet
-- emptySet = S.empty
-- setMember = S.member
-- setInsert = S.insert
data Set = Set { setLow :: !Word64, setHigh :: !Word64 }
emptySet = Set 0 0
setMember i s | i < 64 = testBit (setLow s) i
              | otherwise = testBit (setHigh s) (i - 64)
setInsert i s | i < 64 = s { setLow = setBit (setLow s) i }
              | otherwise = s { setHigh = setBit (setHigh s) (i - 64) }

solve :: Cookbook -> Metals -> Int
solve cb ms0 = evalState (go 0) ms0 where

  go :: Int -> State Metals Int
  go a = do
    recipe <- runMaybeT (accumArray (+) 0 (bounds ms0) <$> produce emptySet 1)
              :: State Metals (Maybe Metals)
    case recipe of
      Nothing -> return a
      Just recipe -> do
        ms <- get
        -- let repeat = minimum ({-# SCC zip #-} zipWith div' ({-# SCC elems_ms #-} elems ms) ({-# SCC elems_recipe #-} elems recipe))
        -- let repeat = minimum [ div' (ms!i) (recipe!i) | i <- indices ms ]
        let repeat = go maxBound 1 where
              go :: Int -> Int -> Int
              go m i | not (inRange (bounds ms) i) = m
                     | m' < m                        = go m' $! i+1
                     | otherwise                     = go m  $! i+1
                where m' = div' (ms!i) (recipe!i)
        put $! accum (-) ms $ map (second (repeat *)) $ assocs recipe
        go $! a + 1 + repeat

  produce :: Set -> Int -> MaybeT (State Metals) [(Int,Int)]
  produce cl m | m `setMember` cl = MaybeT $ return Nothing
               | otherwise = do
    q <- lift $ gets (!m)
    if q > 0 then lift $ modify (flip (accum (+)) [(m,-1)]) >> return [(m,1)]
             else let (ri1,ri2) = cb ! m
                      cl' = setInsert m cl
                  in liftA2 (++) (produce cl' ri1) (produce cl' ri2)

  div' a 0 = maxBound
  div' a b = div a b

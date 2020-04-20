{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad
import Data.Array
import Data.Coerce
import Data.List
import Data.Monoid -- *&^%$# obsolete GHC version at Google's
import Data.Ord
import System.Exit
import System.IO
import qualified Data.ByteString.Char8 as B

newtype Day = Day Int deriving (Eq,Ord,Num)
newtype Vase = Vase Int deriving (Eq,Ord,Enum,Ix,Num,Real,Integral)
newtype Token = Token Int

dayToVase :: Day -> Vase
dayToVase = coerce

data Strat = Stuff1
           | Peek (Array Vase Int)
           | Stuff2 Vase (Array Vase Int)
           | Play Vase
           | Done

readInts :: IO [Int]
readInts = map readInt . B.words <$> B.getLine >>= \case (-1:_) -> exitSuccess
                                                         []     -> exitSuccess
                                                         ints   -> pure ints
  where readInt b = let Just (i,_) = B.readInt b in i

getDay :: IO Day
getDay = readInts >>= \case [day] -> pure (Day day)
                            _     -> exitSuccess

stuff :: Vase -> Int -> IO ()
stuff (Vase v) p = do putStrLn $ show v ++ " " ++ show p
                      hFlush stdout

peek :: Vase -> IO Int
peek v = stuff v 0 *> (head <$> readInts)

stuffing :: Vase
stuffing = 14

peekDate :: Day
peekDate = 60

strategy :: Strat -> Day -> IO Strat
strategy Stuff1 day = do
  let v = 1 + (dayToVase day - 1) `mod` stuffing
  stuff v 100
  pure $ if day+1 < peekDate then Stuff1
                             else Peek (listArray (Vase 1,Vase 20) (repeat (-1)))
strategy (Peek vs) day = do
  let v = dayToVase (day - peekDate + 1)
  n <- peek v
  let vs' = vs // [(v,n)]
  pure $ if v < Vase 20 then Peek vs'
                        else let c = minimumVase vs'
                             in Stuff2 c (vs' // [(c,100)])
strategy (Stuff2 c vs) day = do
  let s = minimumVase vs
  stuff s 100
  pure $ if day + 1 < 100 then Stuff2 c (accum (+) vs [(s,1)])
                          else Play c
strategy (Play c) _ = stuff c 100 *> pure Done
strategy Done _ = exitFailure

minimumVase :: Array Vase Int -> Vase
minimumVase = fst . minimumBy (comparing snd <> comparing (Down . fst)) . assocs

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [t] <- readInts
  replicateM_ t $ 
    foldl' (>>=) (pure Stuff1)
    (replicate 100 (\s -> getDay >>= strategy s))

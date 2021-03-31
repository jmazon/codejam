module Main(main) where

import           Control.Monad (forM_,void)
import           Data.Char     (digitToInt)
import           Data.List     (sortOn)
import           Data.Maybe    (catMaybes)
import qualified Data.Vector as V
import           Data.Vector   (Vector,(!),(!?))

default (Int)

readData :: IO (Vector (Vector Int))
readData = V.replicateM 100 $ V.fromList . map digitToInt <$> getLine

idCheater :: Vector (Vector Int) -> Int
idCheater fs = cheater
  where
    pEase = V.sum <$> fs
    pIndex = V.fromList $ map fst $ sortOn snd $ V.toList $ V.indexed pEase

    qEase = V.foldl1' vAdd fs
    qIndex = V.fromList $ map fst $ sortOn snd $ V.toList $ V.indexed qEase

    ranked = for pIndex $ \i ->
             for qIndex $ \j ->
             fs ! i ! j

    zoom = for ranked $ \qs ->
           V.map V.sum $
           V.unfoldrN 10 (Just . V.splitAt 1000) qs
           -- instead of 100(100) previously

    edges = ifor zoom $ \i z ->
            fmap ((^3) . abs) $
            vAvg $ catMaybes
              [ (z `vSub`) <$> zoom !? (i-1)
              , (z `vSub`) <$> zoom !? (i+1)
              ]

    cheater = pIndex ! V.maxIndex (V.sum <$> edges)

vAdd,vSub :: Vector Int -> Vector Int -> Vector Int
vAdd = V.zipWith (+)
vSub = V.zipWith (-)

vAvg :: [Vector Int] -> Vector Int
vAvg [v] = v
vAvg [u,v] = (`div` 2) <$> vAdd u v

for :: Vector a -> (a -> b) -> Vector b
for  = flip  V.map

ifor :: Vector a -> (Int -> a -> b) -> Vector b
ifor = flip V.imap

main :: IO ()
main = do
  t <- readLn
  void getLine -- p
  forM_ [1..t] $ \i -> do
    fs <- readData
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (1 + idCheater fs)

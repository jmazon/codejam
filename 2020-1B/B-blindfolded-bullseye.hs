{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
import Control.Monad
import Data.Foldable
import System.Exit
import Control.Exception
import System.IO

data NextCase = NextCase deriving (Show,Exception)

shoot :: (Int,Int) -> IO Bool
shoot (x,y) = do
  putStrLn $ show x ++ " " ++ show y
  getLine >>= \case
    "CENTER" -> throwIO NextCase
    "HIT" -> pure True
    "MISS" -> pure False
    "WRONG" -> exitSuccess

main = do
  hSetBuffering stdout LineBuffering
  [t,a,b] <- map read . words <$> getLine
  replicateM_ t $ handle (\NextCase -> pure ()) $ do
    (x0,y0) <- head <$> filterM shoot [ (x,y) | x <- [-5*10^8,0,5*10^8]
                                              , y <- [-5*10^8,0,5*10^8] ]
    l <- bsearchM (\l -> shoot (x0-l,y0)) 0 (x0+10^9+1)
    r <- bsearchM (\r -> shoot (x0+r,y0)) 0 (10^9-x0+1)
    u <- bsearchM (\u -> shoot (x0,y0-u)) 0 (y0+10^9+1)
    d <- bsearchM (\d -> shoot (x0,y0+d)) 0 (10^9-y0+1)
    shoot (x0 + (r-l) `div` 2,y0 + (d-u) `div` 2)
    exitFailure
    pure ()

bsearchM :: (Int -> IO Bool) -> Int -> Int -> IO Int
bsearchM p = go where
  go :: Int -> Int -> IO Int
  go a b = do
    let m = (a + b) `div` 2
    if m == a then pure a else do
      v <- p m
      if v then go m b else go a m

{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad
import Data.Bool
import System.IO
import Control.Monad.Except
import Control.Monad.State.Strict

default (Int)

data Exit = NextCase | BadFormat | WTF | TimeOut | GiveUp deriving Show
type M = StateT Int (ExceptT Exit IO)

crossFingers :: ExceptT Exit IO () -> ExceptT Exit IO ()
crossFingers = flip catchError (\case NextCase -> pure (); e -> throwError e)

shoot :: (Int,Int) -> M Bool
shoot (x,y) = do
  modify' (+1)
  liftIO $ putStrLn $ show x ++ " " ++ show y
  liftIO getLine >>= \case
    "CENTER" -> throwError NextCase
    "HIT"    -> pure True
    "MISS"   -> bool (throwError TimeOut) (pure False) . (< 300) =<< get
    "WRONG"  -> throwError BadFormat
    _        -> throwError WTF

bsearchM :: (Int -> M Bool) -> Int -> Int -> M Int
bsearchM p = go where
  go a b = let m = (a + b) `div` 2 in
           if m == a then pure a else p m >>= bool (go a m) (go m b)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [t,_a,_b] <- map read . words <$> getLine

  ret <- runExceptT $ replicateM_ t $ crossFingers $ flip evalStateT 0 $ do
    (x0,y0) <- head <$> filterM shoot [ (x,y) | x <- [-5*10^8,0,5*10^8]
                                              , y <- [-5*10^8,0,5*10^8] ]
    l <- bsearchM (\l -> shoot (x0-l ,y0))   0 (x0+10^9+1)
    r <- bsearchM (\r -> shoot (x0+r ,y0))   0 (10^9-x0+1)
    u <- bsearchM (\u -> shoot (x0,   y0-u)) 0 (y0+10^9+1)
    d <- bsearchM (\d -> shoot (x0,   y0+d)) 0 (10^9-y0+1)
    void $ shoot (x0 + (r-l) `div` 2,y0 + (d-u) `div` 2)
    throwError GiveUp -- Huh?

  case ret of Left e -> hPrint stderr e
              _ -> pure ()

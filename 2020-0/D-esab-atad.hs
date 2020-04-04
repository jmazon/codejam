{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import System.IO

import Control.Monad.Writer

data Pair = Pair { pairIdx :: Int, pairType :: PairType, pairValue :: Bool }
data PairType = Even | Odd

maybeFlip :: Bool -> Pair -> Pair
maybeFlip True p = p { pairValue = not (pairValue p) }
maybeFlip False p = p

readPair b i = liftIO $ do
  x <- readAt i
  y <- readAt (b+1-i)
  pure Pair { pairIdx = i, pairType = if x == y then Even else Odd
            , pairValue = x}

readAt i = liftIO $ print i *>
                    getLine >>= \case "0" -> pure False
                                      "1" -> pure True
                                      other -> error (unlines [show i,other])

data Knol = Unknown | Ref Pair | Det Pair Bool

clear :: Knol -> Knol
clear (Det p _) = Ref p
clear k = k

merge :: Monad m => Knol -> Pair -> WriterT [Pair] m Knol
merge Unknown p = tell [p] *> pure (Det p False)
merge d@(Det _ b) p = tell [maybeFlip b p] *> pure d

expand :: Int -> Knol -> Knol -> Pair -> [(Int,Bool)]
expand b (Det _ c) _ (Pair i Even v) = [(i,v `xor` c),(b+1-i,v `xor` c)]
expand b _ (Det _ c) (Pair i Odd v) = [(i,v `xor` c),(b+1-i,v `xor` not c)]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [t,b] <- map read . words <$> getLine
  replicateM_ t $ do
    ((re,ro),ps) <- runWriterT (go b Unknown Unknown 0 1)
    let bs = map (intToDigit . fromEnum . snd) $ sort $
             concatMap (expand b re ro) ps
    putStrLn bs
    "Y" <- getLine
    pure ()

go :: Int -> Knol -> Knol -> Int -> Int -> WriterT [Pair] IO (Knol,Knol)
go b re ro _ i | i > b `div` 2 = pure (re,ro)
go b re ro 0 i = go b (clear re) (clear ro) 10 i
go b re@(Det _ e) ro@(Det _ o) n i | n > 1 = do
      p <- readPair b i
      case pairType p of
        Even -> tell [ p { pairValue = pairValue p `xor` e } ]
        Odd -> tell [ p { pairValue = pairValue p `xor` o } ]
      go b re ro (n-2) (i+1)
go b (Ref re) ro n i = do
  x <- readAt (pairIdx re)
  go b (Det re (x `xor` pairValue re)) ro (n-1) i
go b re (Ref ro) n i = do
  x <- readAt (pairIdx ro)
  go b re (Det ro (x `xor` pairValue ro)) (n-1) i
go b re ro n i | n > 1 = do
  p <- readPair b i
  case pairType p of
    Even -> merge re p >>= \re' -> go b re' ro (n-2) (i+1)
    Odd -> merge ro p >>= \ro' -> go b re ro' (n-2) (i+1)
go b re ro 1 i = readAt 1 *> go b re ro 0 i

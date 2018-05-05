import Data.Ord          (comparing)
import Data.Maybe        (isJust)
import Data.Array        (listArray,assocs,accum,(//))
import Data.List         (foldl',sortBy)
import Control.Monad     (when,foldM_)
import Control.Arrow     (first)
import System.IO         (BufferMode(LineBuffering),hSetBuffering,stdout)
import System.Exit       (exitFailure)
import System.Posix.Time (epochTime)
import Foreign.C.Types   (CTime(CTime))
import qualified Data.ByteString.Char8 as B

main = do
  hSetBuffering stdout LineBuffering
  CTime et <- epochTime
  foldM_ (flip ($)) (lcg (fromIntegral et)) .
    flip replicate playTest . readInt =<< B.getLine

playTest rng = do
  n <- readInt <$> B.getLine
  when (n < 0) exitFailure
  let (rs,rng') = splitAt n rng
  foldM_ (flip ($)) (listArray (0,n-1) (map Just $ zip (repeat 0) rs))
         (replicate n playCustomer)
  return rng'

playCustomer stats = do
  (d:wanted) <- map readInt . B.words <$> B.getLine
  when (d < 0) exitFailure
  let stats' = accum (\m i -> fmap (first ((+i))) m) stats $
               map (\c -> (c,1)) wanted
      accepts = filter ((`elem` wanted)) $
                map fst $
                sortBy (comparing snd) $
                filter (isJust . snd) $
                assocs stats'
      (choice,stats'') | null accepts = (-1,stats')
                       | otherwise = (head accepts,stats' // [(choice,Nothing)])
  print choice
  return stats''

lcg = iterate $ \n -> (1103515245 * n + 12345) `mod` 2^31
readInt w | B.null w' = n where Just (n,w') = B.readInt w

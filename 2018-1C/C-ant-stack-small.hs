{-# LANGUAGE FlexibleContexts #-}
import Data.Array
import Data.Array.ST
import qualified Data.IntMap.Strict as M
import Control.Arrow
import Control.Monad.ST
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse
format x y = "Case #" ++ show x ++ ": " ++ show y
parse = go . map read . tail . words where
  go (n:xs) = (n,ws) : go xs' where (ws,xs') = splitAt n xs
  go [] = []
solve n _ws = fst $ runST $ do
  a <- newArray (1,n) M.empty :: ST s (STArray s Int (M.IntMap (Int,Int)))
  let ws = listArray (1,n) _ws
      go i maxW | maxW < 0 = return (minBound,undefined)
                | i == 0 || maxW == 0 = return (0,0)
                | otherwise = do
        m <- readArray a i
        case M.lookupGE maxW m of
          Just (_,v@(n,w)) | w <= maxW -> return v
          _ -> do
            let maxW' = min (maxW - w) (6*w)
                w = ws ! i
                with = ((+1) *** (+w)) <$> go (i-1) maxW'
                without = go (i-1) maxW
            v <- max <$> with <*> without
            writeArray a i (M.insert maxW v m)
            return v
  go n maxBound

import Data.Array    ((!),listArray,accumArray,elems)
import Data.Maybe    (listToMaybe)
import Data.List     (delete,sort)
import Control.Monad (forM_,replicateM,guard)
import qualified Data.BitSet as B

readInts = fmap (map read . words) getLine

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [k,n] <- readInts
    startKeys <- readInts
    rawChests <- replicateM n readInts
    let chestsOpenedBy = accumArray (++) [] (1,200) $
                         zip (map head rawChests) (map return [1..])
        keysInChest = listArray (1,n) $ map (tail . tail) rawChests
        keyOpening  = listArray (1,n) $ map     head      rawChests
        startChests = B.fromList [1..n]
        enoughKeys = all (>= 0) $ elems $ accumArray (+) 0 (1,200) $
                     map (flip (,) 1) (startKeys ++ concat (elems keysInChest))
                  ++ map (flip (,) (-1)) (elems keyOpening)
        solve keys lockedChests trail
            | B.null lockedChests = return (reverse trail)
            | dfs keys lockedChests = fail "broken link"
            | otherwise = cs >>= \c ->
                          solve (keysInChest!c ++ delete (keyOpening!c) keys)
                                (B.delete c lockedChests) (c : trail)
            where cs = sort $ filter (flip B.member lockedChests) $
                       concatMap (chestsOpenedBy!) keys
        dfs _ cs | B.null cs = False
        dfs [] _             = True
        dfs (k:ks) cs        = dfs (ks' ++ ks) (cs B.\\ B.fromList cs')
            where cs' = filter (flip B.member cs) (chestsOpenedBy ! k)
                  ks' = concatMap (keysInChest!) cs'
        result = guard enoughKeys >> solve startKeys startChests []
    putStrLn $ "Case #" ++ show i ++ ": " ++
               maybe "IMPOSSIBLE" (unwords . map show) (listToMaybe result)

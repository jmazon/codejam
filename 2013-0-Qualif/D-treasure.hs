import Data.Array
import Data.List (delete,sort)
import qualified Data.BitSet as B
import Control.Monad (forM_,replicateM,guard)
import Control.Applicative ((<|>))

readInts = fmap (map read . words) getLine

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [k,n] <- readInts
    iKeys <- readInts
    chests <- replicateM n readInts
    let chestsOpenedBy = accumArray (++) [] (1,200) $
                         zip (map head chests) (map (:[]) [1..])
        keysInChest = listArray (1,n) $ map (tail.tail) chests
        keyOpening = listArray (1,n) $ map head chests
        startChests = B.fromList [1..n]
        enoughKeys = all (>= 0) $ elems $ accumArray (+) 0 (1,200) $
                     map (flip (,) 1) (iKeys ++ concat (elems keysInChest)) ++
                     map (flip (,) (-1)) (map head chests)
        r = guard enoughKeys >>
            solve keysInChest chestsOpenedBy keyOpening iKeys startChests
    putStrLn $ "Case #" ++ show i ++ ": " ++
               maybe "IMPOSSIBLE" (unwords . map show) r

solve keysInChest chestsOpenedBy keyOpening = go []
  where go a keys lockedChests
            | B.null lockedChests = Just (reverse a)
            | dfs keys lockedChests = Nothing
            | null cs = Nothing
            | otherwise = foldr (<|>) Nothing $ flip map cs $ \c ->
                          go (c:a) (keysInChest!c ++ delete (keyOpening!c) keys)
                             (B.delete c lockedChests)
            where cs = sort $
                       filter (flip B.member lockedChests) $
                       concatMap (chestsOpenedBy!) keys
        dfs _ cs | B.null cs = False
        dfs [] _             = True
        dfs (k:ks) cs        = dfs (ks' ++ ks) (cs B.\\ B.fromList cs')
            where cs' = filter (flip B.member cs) (chestsOpenedBy ! k)
                  ks' = concatMap (keysInChest!) cs'

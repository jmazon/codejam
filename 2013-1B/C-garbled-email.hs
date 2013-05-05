import Data.Tree (Tree(..))
import Control.Monad.State
import qualified Data.Map.Strict as M

main = do
  trie <- fmap (trieFromAscList . lines)
               (readFile "garbled_email_dictionary.txt")
  t <- readLn
  zipWithM_ format [1..] . map (solve trie) =<< replicateM t getLine

format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ show j

trieFromAscList [] = []
trieFromAscList ([]:xs) = Node '$' [] : trieFromAscList xs
trieFromAscList xs@((c:_):_) = Node c (trieFromAscList $ map tail xs') :
                               trieFromAscList xs''
    where (xs',xs'') = break ((/= c) . head) xs

solve trie xs = evalState (go trie 5 0 xs) M.empty
    where go t i j xs = liftM minimum $ mapM next t
              where next (Node c t')
                      | null xs      = if c == '$' then return (0 :: Int)
                                       else return 5000
                      | c == '$'     = do m <- get
                                          let k = (j,min 5 i)
                                          case M.lookup k m of
                                            Just r -> return r
                                            Nothing -> do
                                              r <- go trie i j xs
                                              modify (M.insert k r)
                                              return r
                      | c == head xs =              go t' (i+1) (j+1) (tail xs)
                      | i >= 5       = liftM (+1) $ go t'   1   (j+1) (tail xs)
                      | otherwise    = return 5000

{-# LANGUAGE FlexibleContexts #-}
import Data.Tree (Tree(..))
import Control.Monad.State
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  trie <- fmap (trieFromAscList . lines)
               (readFile "garbled_email_dictionary.txt")
  t <- readLn
  zipWithM_ format [1 :: Int ..] . map (solve trie) =<< replicateM t getLine

format :: Int -> Int -> IO ()
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ show j

trieFromAscList :: [String] -> [Tree Char]
trieFromAscList [] = []
trieFromAscList ([]:xs) = Node '$' [] : trieFromAscList xs
trieFromAscList xs@((c:_):_) = Node c (trieFromAscList $ map tail xs') :
                               trieFromAscList xs''
    where (xs',xs'') = break ((/= c) . head) xs

solve :: [Tree Char] -> String -> Int
solve trie xs0 = evalState (go trie (5 :: Int) (0 :: Int) xs0) M.empty
    where go t i j xs = liftM minimum $ mapM next t
              where next (Node c t')
                      | null xs      = if c == '$' then return 0 else return 5000
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

import Data.Monoid
import qualified Data.Map as M

main = interact $ unlines . zipWith format [1..] . map solve . parse
parse = go . tail . lines where
  go (n:ls) = ws : go ls' where (ws,ls') = splitAt (read n) ls
  go [] = []
format i j = "Case #" ++ show i ++ ": " ++ show j

solve = getSum . snd . f . buildTrie . map reverse where
  f (Trie t) | M.null t  = (Sum 1,Sum 0)
             | otherwise = mconcat $ map (adj . f) $ M.elems t
  adj (w,d) | w >= 2 = (w-2,d+2)
            | otherwise = (w,d)

newtype Trie = Trie { unTrie :: M.Map Char Trie } deriving Show
buildTrie = foldr insTrie emptyTrie
emptyTrie = Trie M.empty
insTrie   ""   (Trie t) = Trie $ M.insert '$' emptyTrie t
insTrie (c:cs) (Trie t) = Trie $ M.alter (Just . insTrie cs . maybe emptyTrie id)
                                         c t

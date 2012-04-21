import Data.Monoid
import Control.Monad
import qualified Data.Map as M

data Trie = Trie { isTerminal :: Bool
                 , children :: M.Map Char Trie 
                 } deriving Show

tEmpty :: Trie
tEmpty = Trie False M.empty

tInsert :: Trie -> String -> Trie
tInsert t [] = t { isTerminal = True }
tInsert t@(Trie _ cs) (x:xs) = t { children = M.alter f x cs }
    where 
      f :: Maybe Trie -> Maybe Trie
      f Nothing = Just $ tInsert tEmpty xs
      f (Just t') = Just $ tInsert t' xs

tDump :: Trie -> [String]
tDump t@(Trie True c) = "" : tDump t { isTerminal = False }
tDump (Trie False cs) = do
  (c, t') <- M.toList cs
  map (c:) $ tDump t'

tFromList = foldl tInsert tEmpty

reading t "" = if isTerminal t then 1 else 0
reading t ('(':xs) = sum $ map (reading t) attempts
    where (choices, (_:xs')) = break (== ')') xs
          attempts = map (:xs') choices
reading t (x:xs) = case M.lookup x (children t) of
                     Nothing -> 0
                     Just t' -> reading t' xs

main = do
  params <- getLine
  let [l, d, n] = map read $ words params
  words <- replicateM d getLine
  let trie = tFromList words
  -- print $ tDump trie
  readings <- replicateM n getLine
  let counts = map (reading trie) readings
  -- print counts
  forM (zip [1..] counts) $ \(i, c) -> do
         putStrLn $ "Case #" ++ (show i) ++ ": " ++ (show c)


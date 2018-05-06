import Data.Maybe (listToMaybe)
import Data.List  (transpose,sort,group)
main = interact $ unlines . zipWith format [1..] . map solve . parse
parse = go . tail . words where
  go (n:_:xs) = ws : go xs' where (ws,xs') = splitAt (read n) xs
  go [] = []
format x y = "Case #" ++ show x ++ ": " ++ maybe "-" id y
solve ws = listToMaybe $ filter (`notElem` ws) $ sequence $
           map (map head . group . sort) $ transpose ws

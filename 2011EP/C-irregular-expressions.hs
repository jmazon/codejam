import Data.Char
import Data.List
import Control.Monad

isVowel = (`elem` "aeiou")

skipToVowel s = do
  let (a,b) = break isVowel s
  guard (not . null $ b)
  return (a,head b,tail b)

spell s = do
  (_,v1,s') <- skipToVowel s
  (c,v2,s'') <- skipToVowel s'
  (_,_,s''') <- skipToVowel s''
  t <- tails s'''
  guard (not . null $ t)
  guard (isVowel . head $ t)
  (_,v3,t') <- skipToVowel t
  guard (v3 == v1)
  (c',v4,_) <- skipToVowel t'
  guard (c' == c)
  guard (v4 == v2)
  return ()

solve s = case (spell s) of
            [] -> "Nothing."
            _ -> "Spell!"

format i r = "Case #" ++ show i ++ ": " ++ r

main = interact $ unlines . zipWith format [1..] . map solve . tail . words

import Data.Char (isAlpha)

main = interact (unlines . zipWith caseN [1..] . test . lines)

caseN i j = "Case #" ++ show i ++ ": " ++ show j

test (h:t) = map (matches dict) pats
  where [l,d,n] = map read $ words h
        (dict,rpats) = splitAt d t
        pats = map readPat rpats
        
match w p = and $ zipWith elem w p
matches d p = length $ filter (flip match p) d

readPat "" = []
readPat p = map (:[]) s ++ readLit l
  where (s,l) = span isAlpha p
readLit "" = []
readLit p = l : readPat (tail s)
  where (l,s) = span isAlpha (tail p)

-- SUCCESS

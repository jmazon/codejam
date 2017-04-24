import Data.Ord        (Down(Down))
import Data.List.Split (chunksOf)
import Data.List       ((\\),sortOn,find,delete,intersect)

main = interact $ unlines . concat . zipWith format [1..] .
                  map (uncurry3 solve) . parse . tail . words

parse [] = []
parse (_n:_m:ws) = (n,ts,rs) : parse ws'
  where n = read _n
        (ms,ws') = splitAt (3 * read _m) ws
        (ts,rs) = foldr readModel ([],[]) (chunksOf 3 ms)
        readModel [t,_i,_j] (rs,bs) = (rs',bs')
          where rs' = if t == "+" then rs else pos : rs
                bs' = if t == "x" then bs else pos : bs
                pos = (read _i,read _j)

format x (s,ms) =
  ("Case #" ++ show x ++ ": " ++ show s ++ " " ++ show (length ms)) :
  map (unwords . (\(t,(i,j)) -> [t,show i,show j])) ms

solve n rooks bishops = (score,rooks' ++ bishops' ++ queens) where
  newRooks = solveRooks rooks
  newBishops = map fromDiags $ solveBishops $ map toDiags bishops
  toDiags (i,j) = (i - j, i + j - n - 1)
  fromDiags (d1,d2) = ((d1 + d2 + n + 1) `div` 2,(d2 - d1 + n + 1) `div` 2)
  score = length rooks + length bishops + length newRooks + length newBishops
  rooks'   = ((,) "x") <$> (newRooks \\ bishops) \\ newBishops
  bishops' = ((,) "+") <$> (newBishops \\ rooks) \\ newRooks
  queens   = ((,) "o") <$> intersect newRooks bishops ++
                           intersect newBishops rooks ++
                           intersect newRooks newBishops

  solveRooks rs = zip is' js'
    where (is,js) = unzip rs
          is' = [1..n] \\ is
          js' = [1..n] \\ js

  solveBishops bs = go is' js'
    where (is,js) = unzip bs
          is' = sortOn (Down . abs) ([-n+1..n-1] \\ is)
          js' = [-n+1..n-1] \\ js
          go [] _ = []
          go (i:is) js | Just j <- find ((< n) . dist i)  $
                                   sortOn (Down . dist i) $
                                   filter (even . (+ (n-1+i))) js =
                           (i,j) : go is (delete j js)
                       | otherwise = go is js
          dist i j = abs i + abs j

uncurry3 f (a,b,c) = f a b c

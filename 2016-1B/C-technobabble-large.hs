import Data.Graph.MaxBipartiteMatching
import qualified Data.Map as M
import qualified Data.Set as S
main = interact $ unlines . zipWith format [1..] . map (uncurry solve) . parse . tail . lines
format i j = "Case #" ++ show i ++ ": " ++ show j
parse [] = []
parse (_n:ls) = (n,map pair ts) : parse ls'
  where n = read _n; (ts,ls') = splitAt n ls
pair l = (a,b) where [a,b] = words l
solve n ps = let m = matching (S.fromList ps)
                 (w1s,w2s) = unzip ps
             in n - M.size m
                  - S.size (S.fromList w1s S.\\ S.fromList (M.elems m))
                  - S.size (S.fromList w2s S.\\ M.keysSet m)

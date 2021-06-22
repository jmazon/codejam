-- for Data.Graph.MaxBipartiteMatching
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List ( foldl' )

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

-- Next two functions are Data.Graph.MaxBipartiteMatching
-- from hackage's maxBipartiteMatching-0.1.0.0

matching :: (Ord a, Ord b) => S.Set (a,b) -> M.Map b a
matching g = opt (M.keys fwd, []) fwd M.empty
    where
    fwd = foldl' (\m (x,y) -> M.insertWith (++) x [y] m) M.empty $ S.toList g

opt :: (Ord a, Ord b) => ([a],[a]) -> M.Map a [b] -> M.Map b a -> M.Map b a
opt (x:free,failed) fwd mat
  = either (flip (opt (free,x:failed)) mat) (opt (free++failed,[]) fwd)
    $ right fwd [] x
  where
    right rem path x
      = maybe (Left rem) (left $ M.delete x rem) $ M.lookup x rem
      where
        left rem [] = Left rem
        left rem (y:ys)
          = maybe
            (Right $ foldr (uncurry $ flip M.insert) mat path') --A
            (either (flip left ys) Right . right rem path')     --B
            (M.lookup y mat)                                    --C
          where
            path' = (x,y):path
opt ([],failed) fwd mat = mat

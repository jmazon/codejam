import Data.List
import qualified Data.IntMap.Strict as M
import Control.Monad
main = zipWithM_ format [1..] . map (uncurry solve) . unfoldr parse . map read . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ if j then "YES" else "NO"
parse [] = Nothing
parse (n:ns) = Just ((d,pairs vs),ns') where (vs,(d:ns')) = splitAt (2*n) ns
pairs (x:y:zs) = (x,y) : pairs zs
pairs _ = []
solve t ((d,_):vs) = solve' t (M.fromAscList vs) (M.singleton d d)
solve' t vs hs | M.null hs  = False
               | reach >= t = True
               | otherwise  = solve' t vs $
                              M.union hs' $
                              M.mapWithKey (\d' l' -> min (d' - d) l') $
                              snd (M.split (fst (M.findMax hs)) 
                                           (fst (M.split (reach+1) vs)))
    where ((d,l),hs') = M.deleteFindMin hs
          reach = d + l

import Data.Ord
import Data.List
import Control.Monad
main = zipWithM format [1..] . map solve . parse . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ maybe "Too damaged" show' j
solve = flip find (sortBy (comparing manhattan) $ reverse $ sortBy (comparing fst) $ liftM2 (,) [-201..201] [-201..201]) . fits
parse (n:ns) = unfoldr go ts : parse ns'
    where (ts,ns') = splitAt (3 * read n) ns
          go ns = do (x:y:c:ns') <- Just ns; Just (((read x,read y),c == "#"),ns')
fits ts (x,y) = all ok ts
    where ok ((xi,yi),ci) = ci == odd (max (abs (xi-x)) (abs (yi-y)))
manhattan (x,y) = abs x + abs y
show' (x,y) = show x ++ " " ++ show y

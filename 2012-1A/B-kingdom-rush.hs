import Control.Monad
import Data.List
import Data.Ord
main = zipWithM_ format [1..] . map solve . unfoldr parse .
       map read . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ maybe "Too Bad" show j
parse (n:ns) = let (ns',ns'') = splitAt (2*n) ns in Just (pairs ns',ns'')
parse _ = Nothing
pairs (x:y:xs) = (x,y) : pairs xs
pairs _ = []
solve l = solve' l [] 0 0
solve' f h p g | not (null easyF) = solve' hardF     h (p + 2*nef) (g + nef)
               | not (null easyH) = solve' hardF hardH (p +   neh) (g + neh)
               | not (null easyHardF) = solve' (delete bestF hardF) 
                                               (bestF:h)
                                               (p+1) (g+1)
               | not (null hardF) || not (null hardH) = Nothing
               | otherwise = Just g
    where (easyF,hardF) = partition ((<=p) . snd) f; nef = length easyF
          (easyH,hardH) = partition ((<=p) . snd) h; neh = length easyH
          easyHardF = filter ((<=p) . fst) hardF
          bestF = maximumBy (comparing snd) easyHardF

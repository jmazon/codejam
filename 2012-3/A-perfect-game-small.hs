import Data.Ord
import Data.List
import Control.Monad
main = zipWithM_ format [1..] . map solve . unfoldr parse . map read . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ unwords (map show j)
parse (n:ns) = let (ls,ls') = splitAt n ns
                   (ps,ps') = splitAt n ls'
               in Just (zip ls ps,ps')
parse _ = Nothing
solve = map fst . sortBy (comparing (negate . snd . snd)) . zip [0..]

import Control.Monad
import Data.List
import Data.Array
import qualified Data.IntMap.Strict as M
main = zipWithM_ format [1..] . map solve . unfoldr parse .
       map read . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++
             if j then "Yes" else "No"
parse (n:ns) = parse' n ns []
parse _ = Nothing
parse' 0 ns a = Just (reverse a,ns)
parse' j (n:ns) a = let (ps,ns') = splitAt n ns in parse' (j-1) ns' (ps:a)
solve p = not (null (filter hasDiamond (elems ps)))
    where as = zip [1..] p
          a = accumArray (++) [] (1,length p) as
          ps :: Array Int (M.IntMap Int)
          ps = listArray (1,length p) (map parents p)
          parents :: [Int] -> M.IntMap Int
          parents p = M.unionWith (+)
                      (M.unionsWith (+) (map (ps!) p))
                      (M.fromList [ (d,1) | d <- p ])
          hasDiamond m = not (M.null (M.filter (> 1) m))

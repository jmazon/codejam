import Data.Maybe
import Data.Array
import Data.List
import Control.Monad

import Control.Exception

main = interact $ unlines . zipWith c [1..] . map solve . parse . map read . tail . words
c i j = "Case #" ++ show i ++ ": " ++ present j
parse [] = []
parse (n:m:ns) = (n,is,js) : parse ns'
    where (ijs,ns') = splitAt (2*m) ns
          (is,js) = splitAt m ijs
present = intercalate " " . map show . catMaybes . elems

chained [_] = True
chained (a:b:xs) = case intersect a b of
                     (_:_:_) -> chained (b:xs)
                     _ -> False

rooms :: Int -> [Int] -> [Int] -> [[Int]]
rooms n is js = g $ foldl' f [[1..n]] (zip is js)
    where f rs (i,j) = r1:r2:r `delete` rs
              where Just r = find (\r -> i `elem` r && j `elem` r) rs
                    r' = cycle r
                    r1 = j : takeWhile (/=j) (dropWhile (/=i) r')
                    r2 = i : takeWhile (/=i) (dropWhile (/=j) r')
          g (r:rs) = r : unfoldr g' (r,rs)
          g' (vs,rs) = find t rs >>= \r -> return (r,(union r vs,r `delete` rs))
              where t r = case filter (flip elem r) vs of
                            (_:_:_) -> True
                            _ -> False

solve :: (Int,[Int],[Int]) -> Array Int (Maybe Int)
solve (n,is,js) = foldl' f a1 rs
    where (r:rs) = rooms n is js
          m = minimum (map length rs)
          a0 = listArray (1,n) (repeat Nothing)
          a1 = a0 // zip r (map Just $ cycle [1..m])
          f :: Array Int (Maybe Int) -> [Int] -> Array Int (Maybe Int)
          f a r = (a //) . head $ do
            let ucs = nub $ catMaybes [ a!v | v <- r ]
                rcs = [1..m] \\ ucs
                go :: Int -> Bool -> [Int] -> [Int] -> [[(Int,Maybe Int)]]
                go x _      [] vs = go x True [1..m] vs
                go _ True _ []    = return []
                go _ False _ []   = fail "out of colors"
                go x f cs (v:vs) =
                    case a!v of
                      Just c -> guard (c /= x) >> go c f cs vs
                      Nothing -> do
                               c <- filter (\c -> c /= x) cs
                               as <- go c f (c `delete` cs) (vs)
                               return ((v,Just c):as)

                r' = uncurry (flip (++)) (break (isJust . (a!)) r) ++ [head r']
            go 0 False rcs r'

-- FAIL

import Data.List (unfoldr,inits,tails)
import Control.Monad (zipWithM_,guard)
main = zipWithM_ format [1..] . map solve . parse . map read . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ show j
dist x1 y1 x2 y2 = max (abs (x2 - x1)) (abs (y2 - y1))
solve = maximum . (0:) . smashable True 0 0 0 0
    where smashable c x y t n zs = do
            ((x',y',ta),zs') <- pick zs
            let td = ta + 1000
                tr = t + max (if c then 0 else 750) (100 * dist x y x' y')
                n' = n + 1 :: Int
                tk = max ta tr
            guard $ tr <= td
            n' : smashable False x' y' tk n' zs'
parse (n:ns) = unfoldr toTriplets ts : parse ns'
    where (ts,ns') = splitAt (3*n) ns
          toTriplets (a:b:c:xs) = Just ((a,b,c),xs)
          toTriplets _ = Nothing
parse _ = []
pick xs = zip xs $ zipWith (++) (init (inits xs)) (tail (tails xs))

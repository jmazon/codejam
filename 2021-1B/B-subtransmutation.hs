import           Control.Monad (forM_,guard)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap
import           Data.List (find)

default (Int)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [_n,a,b] <- map read . words <$> getLine
    us <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++
      maybe "IMPOSSIBLE" show (solve a b us)

solve :: Int -> Int -> [Int] -> Maybe Int
solve a b us = guard (all ((== 0) . (`mod` gcd a b)) δs) *>
               find (valid a b us') [0..]
  where δs = map =<< subtract . head $ IMap.keys us'
        us' = IMap.filter (> 0) $ IMap.fromList $ zip [1..] us

-- | Given a starting metal, can we fulfil the rare metal need?
valid :: Int -> Int -> IntMap Int -> Int -> Bool
valid a b us n = go us (IMap.singleton n 1) where
  go n s
    | IMap.null n = True      -- no metal need → success
    | IMap.null s = False     -- no metal in stock → failure
    | w < nmax    = False     -- heaviest needed is heavier than stock → failure
    | w > nmax    = go n s''  -- heaviest stock isn't needed → transmute it
    | nw < unmax  = False     -- not enough heaviest metal produced → failure
    | otherwise   = go n' s'' -- reserve and transmute
    where
      (nmax,unmax) = IMap.findMax n            -- heaviest needed metal
      Just ((w,nw),s') = IMap.maxViewWithKey s -- heaviest in stock
      cons | w == nmax = min nw unmax           -- consumed quantity
           | otherwise = 0
      -- quantities after consumption and transmutation
      s'' = IMap.unionWith (+) s' $
            IMap.fromList [ (c,nw-cons) | c <- [w-a,w-b], c > 0 ]
      -- needed quantities after consumption
      n' = IMap.delete nmax n

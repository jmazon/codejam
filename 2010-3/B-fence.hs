import Data.List
import Data.Functor
import Data.Maybe
import Control.Monad

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    [l,n] <- map read . words <$> getLine
    bs <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show t ++ ": " ++ show' (solve l bs)

solve l bs = solve' l bs' gs
    where bs' = reverse (sort bs)
          gs = scanr1 gcd bs'

solve' :: Integer -> [Integer] -> [Integer] -> Maybe Integer
solve' 0 _ _ = Just 0
solve' l (b:bs) (g:gs) | l `mod` g /= 0 = Nothing
                       | otherwise      = minimum' . catMaybes $ do
  let (q,r) = l `divMod` b
  (n,l') <- zip [q,q-1..0] [r,r+b..]
  return ((+n) <$> solve' l' bs gs)

head' (x:_) = Just x
head' _     = Nothing

minimum' [] = Nothing
minimum' xs = Just $ minimum xs

show' Nothing = "IMPOSSIBLE"
show' (Just a) = show a

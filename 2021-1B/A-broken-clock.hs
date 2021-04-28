import Control.Monad (forM_,guard)
import Data.List     (permutations)
import Data.Maybe    (mapMaybe)

default (Int)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [a,b,c] <- map read . words <$> getLine
    let (h,m,s,n) = solve a b c
    putStrLn $ "Case #" ++ show i ++ ": " ++ unwords (map show [h,m,s,n])

solve :: Integer -> Integer -> Integer -> (Integer,Integer,Integer,Integer)
solve a b c = (h,m,s,n) where
  (t',n) = t `divMod` (10^9)
  (t'',s) = t' `divMod` 60
  (h,m) = t'' `divMod` 60
  t = head $ mapMaybe verify $ permutations [a,b,c]

verify :: [Integer] -> Maybe Integer
verify [h,m,s] = do
  let t = (m - h) * eleven_ `mod` twoPi
  guard $ ((720-1) * t - (s - h)) `mod` twoPi == 0
  guard $ ((720-12) * t - (s - m)) `mod` twoPi == 0
  pure t

twoPi,eleven_ :: Integer
twoPi = 360 * 12 * 10^10
eleven_ = 15709090909091

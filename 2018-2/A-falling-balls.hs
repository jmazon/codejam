module Main (main) where

import Data.Maybe
import Control.Monad
import Control.Applicative

-- All lists in this problem have length C ≥ 2

solve :: [Int] -> Maybe [String]
solve = fmap draw . mfilter valid . pure . unramp where
  unramp = concat . zipWith (flip replicate) [1..]
  valid = liftA2 (&&) head last . zipWith (==) [1..]
  draw targets = map row [h,h-1..0] where
    h = maximum $ zipWith ((abs .) . (-)) targets [1..]
    row i = zipWith (\_ j -> fromMaybe '.' $ lookup j (rowEls i)) targets [1..]
    rowEls i = catMaybes $ zipWith (col i) [1..] targets
  col i j t = guard (i > 0 && i <= abs (t - j)) *>
    case signum (t - j) of -1 -> Just (t+i,'/' )
                           1  -> Just (t-i,'\\')

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    getLine -- c
    bs <- map read . words <$> getLine
    putStr $ "Case #" ++ show i ++ ": "
    case solve bs of
      Nothing -> putStrLn "IMPOSSIBLE"
      Just ls -> print (length ls) *> mapM_ putStrLn ls

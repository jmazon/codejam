import Control.Monad
import Data.Char
import Data.List
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    s <- map digitToInt <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ unfoldr f (s,0)
f ([],0) = Nothing
f ([],i) = Just (')',([],i-1))
f (r:rs,d) = case compare d r of
  LT -> Just ('(',(r:rs,d+1))
  EQ -> Just (intToDigit d,(rs,d))
  GT -> Just (')',(r:rs,d-1))

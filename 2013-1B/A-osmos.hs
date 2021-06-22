import Data.List
import Control.Monad.State
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [a,_n] <- fmap (map read . words) getLine
    ns <- fmap (map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++
               show (evalState (solve 0 a (sort ns)) M.empty)

solve :: Int -> Int -> [Int] -> State (M.Map (Int,Int) Int) Int
solve _ _ [] = return (0 :: Int)
solve c a (n:ns) = do
  m <- get
  case M.lookup (c,a) m of
    Just r -> return r
    Nothing -> do
      r <- if a > n then solve (c+1) (a+n) ns
           else liftM2 min
                  (fmap succ $ solve (c+1) a ns)
                  (if a > 1 then fmap succ $ solve c (2*a - 1) (n:ns)
                   else return maxBound)
      modify (M.insert (c,a) r)
      return r


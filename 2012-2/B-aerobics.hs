import Data.Function
import Data.List
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
main = do
  t <- readLn
  g <- getStdGen
  flip evalRandT g $ forM_ [1..t] $ \i -> do
    [_,w,l] <- lift $ liftM (map read . words :: String -> [Int]) getLine
    rs <- lift $ liftM (reverse . sortBy (compare `on` snd) . zip [0..] . map read . words) getLine
    s <- liftM (map snd . sortBy (compare `on` fst)) $ solve w l rs
    lift $ putStrLn $ "Case #" ++ show i ++ ": " ++
                      unwords (map (\(x,y,_) -> show x ++ " " ++ show y) s)
solve w l rs = solve' w l rs [] >>= either (const $ solve w l rs) return
solve' w l [] a = return (Right a)
solve' w l ((i,r):rs) a = flip fix 100 $ \f n -> do
  x <- getRandomR (0,w)
  y <- getRandomR (0,l)
  if all (\(_,(x',y',r')) -> (x'-x)^2 + (y'-y)^2 >= (r'+r)^2) a
  then solve' w l rs ((i,(x,y,r)):a)
  else if n == 0 then return (Left ())
  else f (n-1)
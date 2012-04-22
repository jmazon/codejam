import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import System.IO

solve l p c = fromJust $ findIndex (<=c) $ iterate sqrt (p/l)

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    [l,p,c] <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show t ++ ": " ++ show (solve l p c)

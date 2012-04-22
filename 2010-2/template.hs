import Data.Functor
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \t -> do
    putStrLn $ "Case #" ++ show t ++ ": " ++ show 0

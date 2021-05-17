import Control.Monad
import System.Exit
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [t,n] <- map read . words <$> getLine
  replicateM_ t $ do
    forM_ [1..n-1] $ \i -> do
      send $ "M " ++ show i ++ " " ++ show n
      m <- get
      when (m > i) $ do
        send $ "S " ++ show i ++ " " ++ show m
        1 <- get
        pure ()
    send "D"
    1 <- get
    pure ()

send :: String -> IO ()
send = putStrLn

get :: IO Int
get = do
  n <- readLn
  when (n < 0) exitSuccess
  pure n

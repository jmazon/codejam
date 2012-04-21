import qualified Data.Map as M
import Data.Function
import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    [p,q] <- map read . words <$> getLine
    qs <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (bribe' p qs)

bribe :: Int -> [Int] -> State (M.Map (Int,[Int]) Int) Int
bribe _ [] = return 0
bribe p qs = do
  c <- get
  case M.lookup (p,qs) c of
    Just r -> return r
    Nothing -> do
      r <- liftM minimum $ sequence $ do
        q <- qs
        let (qs1, (_:qs2)) = break (>= q) qs
        return $ do
          b1 <- bribe (q-1) qs1
          b2 <- bribe (p-q) $ map (flip (-) q) qs2
          return $ (p-1) + b1 + b2
      modify $ \m -> M.insert (p,qs) r m
      return r

bribe' :: Int -> [Int] -> Int
bribe' p qs = flip evalState M.empty $ bribe p qs
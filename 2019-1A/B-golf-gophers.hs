import Control.Monad
import System.IO
import System.Exit

primes = [18,5,7,11,13,17]

main = do
  hSetBuffering stdout LineBuffering
  [t,n,m] <- map read . words <$> getLine :: IO [Int]
  replicateM_ t $ do
    rs <- forM primes $ \p -> do
      putStrLn $ unwords $ map show $ replicate 18 p
      m <- sum . map read . words <$> getLine
      when (m < 0) exitSuccess
      return $ m `mod` p
    print $ chinese rs
    r <- readLn
    when (r < 0) exitSuccess

-- Yes, I *know* there's a smarter way to do this!
chinese rs = head $ filter valid [1..10^6]
  where valid n = map (mod n) primes == rs

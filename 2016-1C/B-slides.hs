import Data.Bits     (testBit)
import Control.Monad (forM_,when)

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [b,m] <- map read . words <$> getLine
    let possible = m <= 2 ^ (b-2)
    putStrLn $ "Case #" ++ show i ++ ": " ++
                if possible then "POSSIBLE" else "IMPOSSIBLE"
    when possible $ do
      forM_ [1..b] $ \i -> do
        forM_ [1..b] $ \j ->
          putChar $ if j > i && (j < b || testBit m (i-2) || m == 2 ^ (b-2))
                    then '1' else '0'
        putChar '\n'

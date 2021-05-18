import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO Int
  let cacheSmall = solve 1000
      cacheLarge = solve 1000000
  forM_ [1..t] $ \i -> do
    n <- readLn
    let cache | n <= 1000 = cacheSmall
              | otherwise = cacheLarge
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (cache ! n)

-- | Compute a solution look-up table up to N.
solve :: Int -> UArray Int Int
solve hi = runSTUArray $ do
  ns <- newArray (1,hi) 0
  let go sides polygons s = do
        let sides' = sides+s
            polygons' = polygons+1
        when (sides' <= hi) $ do
          n <- readArray ns sides'
          let n' = max polygons' n
          writeArray ns sides' n'
          mapM_ (go sides' polygons') [2*s,3*s..hi-sides']
  mapM_ (go 0 0) [3..hi]
  pure ns

import           Control.Monad (forM_)
import           Data.List (inits)
import           Data.Maybe (fromMaybe)
import qualified Data.IntSet as S

roarFrom :: Int -> [Int]
roarFrom n =
    map fromIntegral $
    takeWhile (<= 1234567891011121314) $
    drop 2 $
    scanl append 0 [fromIntegral n..]
  where
    append a k = a * decimalWidth k + k :: Integer
    decimalWidth k = head $ filter (k <) $ iterate (* 10) 1

brute :: Int -> Int
brute =
  let cache = S.fromList (concatMap roarFrom [1..999])
  in \i -> fromMaybe (error "Not found")
          (S.lookupGT (fromIntegral i) cache)

specials :: [Int]
specials =
  concatMap roarFrom [ max 1 (10^i-k) | i <- [1..18 :: Int], k <- [0..18] ]
    
fine :: Int -> Int
fine y = minimum (filter (> y)
                   (concatMap roarFrom (concatMap approx (tail (inits (show y))))
                     ++ specials))
  where
    approx s = let n = read s in [n-1,n,n+1]

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    y <- readLn
    let solve | y <= 1000000 = brute
              | otherwise   = fine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve y)

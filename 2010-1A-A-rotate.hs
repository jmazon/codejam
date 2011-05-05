import Data.Array
import Data.List
import Data.Maybe
import Control.Monad

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [n,k] <- (map read . words) `fmap` getLine
    b <- ( winner n k .
           listArray ((0,0),(n-1,n-1)) .
           concatMap ( take n . (++ repeat Nothing) .
                       reverse . filter isJust . map decode) )
         `fmap` replicateM n getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ b

decode '.' = Nothing
decode 'R' = Just True
decode 'B' = Just False

winner n k b = case nub . catMaybes $ h ++ v ++ d1 ++ d2 of
                 [] -> "Neither"
                 [True] -> "Red"
                 [False] -> "Blue"
                 _ -> "Both"
    where h = [c | i <- [0..n-k], j <- [0..n-1],
                   let c = b!(j,i),
                   all (==c) [b!(j,i+d) | d <- [1..k-1]]]
          v = [c | j <- [0..n-k], i <- [0..n-1],
                   let c = b!(j,i),
                   all (==c) [b!(j+d,i) | d <- [1..k-1]]]
          d1 = [c | j <- [0..n-k], i <- [0..n-k],
                    let c = b!(j,i),
                    all (==c) [b!(j+d,i+d) | d <- [1..k-1]]]
          d2 = [c | j <- [0..n-k], i <- [k-1..n-1],
                    let c = b!(j,i),
                    all (==c) [b!(j+d,i-d) | d <- [1..k-1]]]

-- SUCCESS

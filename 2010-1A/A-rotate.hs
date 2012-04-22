import Data.Array
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    (b,n,k) <- getBoard
    let b' = squeeze b n
    putStrLn $ "Case #" ++ show i ++ ": " ++ determine (isWinner b' n k False)
                                                       (isWinner b' n k True)
    --drawBoard b' n

getBoard = do
  [n,k] <- map read . words <$> getLine
  b <- listArray ((1,1),(n,n)) . map decodeBoard . concat <$>
       replicateM n getLine
  return (b,n,k)

decodeBoard '.' = Nothing
decodeBoard 'R' = Just True
decodeBoard 'B' = Just False

encodeBoard Nothing = '.'
encodeBoard (Just True) = 'R'
encodeBoard (Just False) = 'B'

squeeze b n = listArray (bounds b) $ concatMap (take n . row') [1..n]
    where row i = reverse $ filter isJust $ map (\j -> b!(i,j)) [1..n]
          row' i = row i ++ repeat Nothing

isWinner b n k c = isHWinner b n k c
                   || isVWinner b n k c
                   || isD1Winner b n k c
                   || isD2Winner b n k c

isHWinner b n k c = any hAt [ (i,j) | i <- [1..n], j <- [1..n-k+1] ]
    where hAt (i,j) = all (== Just c) [ b!(i,j+x) | x <- [0..k-1] ]
isVWinner b n k c = any vAt [ (i,j) | i <- [1..n-k+1], j <- [1..n] ]
    where vAt (i,j) = all (== Just c) [ b!(i+x,j) | x <- [0..k-1] ]
isD1Winner b n k c = any dAt [ (i,j) | i <- [1..n-k+1], j <- [1..n-k+1] ]
    where dAt (i,j) = all (== Just c) [ b!(i+x,j+x) | x <- [0..k-1] ]
isD2Winner b n k c = any dAt [ (i,j) | i <- [k..n], j <- [1..n-k+1] ]
    where dAt (i,j) = all (== Just c) [ b!(i-x,j+x) | x <- [0..k-1] ]


-- debug
drawBoard b n = forM_ [1..n] $ \i ->
                putStrLn $ map (f i) [1..n]
    where f i j = encodeBoard $ b ! (i,j)

determine False False = "Neither"
determine False True  = "Red"
determine True False  = "Blue"
determine True True   = "Both"
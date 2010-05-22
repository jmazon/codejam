import Control.Applicative
import Control.Monad
import System.IO

winning a b = case a `compare` b of
                EQ -> False
                LT -> winning b a
                GT -> a > 2*b || not (winning b (a-b))

main = do
  hSetBuffering stdout LineBuffering
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    [a1, a2, b1, b2] <- map read . words <$> getLine
    let solver = if False then small else large
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solver a1 a2 b1 b2)

small a1 a2 b1 b2 = fromIntegral $ length $ filter id $ winning <$> [a1..a2] <*> [b1..b2]

phi = (1 + sqrt 5) / 2
large :: Int -> Int -> Int -> Int -> Integer
large a1 a2 b1 b2
    | b2-b1 < a2-a1 = large b1 b2 a1 a2
    | otherwise     = sum [ fromIntegral $ zone1 + zone2 |
                            a <- [a1..a2],
                            let bl = floor (fromIntegral a/phi),
                            let bh = ceiling (fromIntegral a*phi),
                            let zone1 = max 0 (min b2 bl - b1 + 1),
                            let zone2 = max 0 (b2 - max bh b1 + 1) ]

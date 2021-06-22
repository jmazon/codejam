import Control.Monad (forM_,void)
import Data.Char     (digitToInt)
import Data.Function (on)
import Data.List     (isPrefixOf)

data Number = N
  { nRaw :: [Int]
  , nCooked :: Integer
  , nLength :: Int
  }

fromString :: String -> Number
fromString s = N (digitToInt <$> s) (read s) (length s)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    void getLine -- n
    xs <- map fromString . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve xs)

solve :: [Number] -> Int
solve (x:xs) = snd $ foldl f (x,0) xs where
  f (prev,acc) next
    | prev < next = (next,acc)
    | n' <- succ prev, next `nbPrefixOf` n' = (n',acc + nLength n' - nLength next)
    | prev < smallPad = (smallPad,acc + pad)
    | otherwise = (largePad,acc + pad + 1)
    where pad = nLength prev - nLength next
          smallPad = zeroPad next pad
          largePad = zeroPad smallPad 1

instance Eq Number where (==) = (==) `on` nCooked
instance Ord Number where compare = compare `on` nCooked
instance Enum Number where
  toEnum = fromInt . fromIntegral
  fromEnum = fromIntegral . nCooked

fromInt :: Integer -> Number
fromInt n = let s = show n in N (digitToInt <$> s) n (length s)

nbPrefixOf :: Number -> Number -> Bool
nbPrefixOf = isPrefixOf `on` nRaw

zeroPad :: Number -> Int -> Number
zeroPad (N raw cooked lg) pad =
  N (raw ++ replicate pad 0) (cooked * 10^pad) (lg+pad)

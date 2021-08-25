import Control.Monad (forM_,void)
import Data.Char     (digitToInt)
import Data.Function (on)
import Data.List     (isPrefixOf)

data Number = N
  { nRaw    :: [Int]    -- ^ digits
  , nCooked :: Integer  -- ^ value
  , nWidth  :: Int      -- ^ width
  }

fromString :: String -> Number
fromString s = N (digitToInt <$> s) (read s) (length s)

instance Eq Number where (==) = (==) `on` nCooked
instance Ord Number where compare = compare `on` nCooked

nbSucc :: Number -> Number
nbSucc (N _ cooked _) = N ds cooked' (length ds)
  where cooked' = succ cooked
        ds = digitToInt <$> show cooked'

nbPrefixOf :: Number -> Number -> Bool
nbPrefixOf = isPrefixOf `on` nRaw

zeroPad :: Number -> Int -> Number
zeroPad (N raw cooked width) pad =
  N (raw ++ replicate pad 0) (cooked * 10^pad) (width + pad)

solve :: [Number] -> Int
solve (x:xs) = snd $ foldl f (x,0) xs where
  f (prev,acc) next
    | prev < next = (next,acc)
    | n' <- nbSucc prev, next `nbPrefixOf` n' = (n',acc + nWidth n' - nWidth next)
    | prev < paddedSmall = (paddedSmall,acc + padWidth)
    | otherwise          = (paddedLarge,acc + padWidth + 1)
    where padWidth = nWidth prev - nWidth next
          paddedSmall = zeroPad next padWidth
          paddedLarge = zeroPad paddedSmall 1

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    void getLine -- n
    xs <- map fromString . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve xs)

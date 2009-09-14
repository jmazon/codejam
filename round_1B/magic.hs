import Data.Function
import Data.List
import Data.Array
import Control.Applicative
import Control.Monad

data Cell = Op Op | Digit Int deriving (Eq, Ord)
data Op = Plus | Minus deriving (Eq, Ord)

instance Show Cell where
    show (Digit d) = show d
    show (Op o) = show o

instance Show Op where
    show Plus = "+"
    show Minus = "-"

isDigit (Digit _) = True
isDigit _ = False

fromDigit (Digit d) = d

fromOp (Op o) = o

apply Plus d = d
apply Minus d = -d

sbounds :: ((Int,Int), (Int,Int))
sbounds = ((0,0), (4,4))
sample = array sbounds $ zip (range sbounds) $
         [ Digit 2, Op Plus, Digit 1, Op Minus, Digit 2,
           Op Plus, Digit 3, Op Minus, Digit 4, Op Plus,
           Digit 5, Op Plus, Digit 2, Op Plus, Digit 1,
           Op Minus, Digit 4, Op Minus, Digit 0, Op Minus,
           Digit 9, Op Plus, Digit 5, Op Plus, Digit 1 ]

neighbors b (i,j) = filter (inRange b) $
                    [ (i-1, j), (i+1, j), (i, j-1), (i, j+1) ]

data Path = Path { value :: Int,
                   expr :: String,
                   tip :: (Int, Int) } deriving (Show)

singles s = map p $ filter isDigit $ elems s
    where p (Digit d) = Path d (show d) (-1,-1)

starts s = do
  (c, v) <- filter (isDigit . snd) $ assocs s
  c' <- neighbors (bounds s) c
  let op = fromOp $ s!c'
  let v' = apply op $ fromDigit v
  return (Path v' (show op ++ show v) c')

concluders s p = do
  c <- neighbors (bounds s) (tip p)
  let v = fromDigit $ s!c
  return p { value = value p + v,
             expr = (show) v ++ expr p,
             tip = c }

extenders s p = do
  c <- neighbors (bounds s) (tip p)
  let v = fromDigit $ s!c
  c' <- neighbors (bounds s) c
  let op = fromOp $ s!c'
  return p { value = value p + apply op v,
             expr = (show op) ++ (show v) ++ expr p,
             tip = c' }

search s q = if q `elem` (map value $ singles s)
             then (show q)
             else search' s q (starts s)

search' s q l = case find ((==q) . value) ccs of
                  Just p -> expr p
                  Nothing -> search' s q l'
    where ccs = sortBy (compare `on` expr) $  l >>= (concluders s)
          l' = l >>= extenders s

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
         putStrLn $ "Case #" ++ (show i) ++ ":"
         [w,q] <- (read <$>) . words <$> getLine
         s <- readSqr w
         qs <- (read <$>) . words <$> getLine
         mapM_ (putStrLn . (search s)) qs

readSqr w = listArray ((1,1), (w,w)) . 
            map convert .
            concat <$>
            replicateM w getLine
    where convert :: Char -> Cell
          convert '+' = Op Plus
          convert '-' = Op Minus
          convert d = Digit $ fromEnum d - fromEnum '0'

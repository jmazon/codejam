import Data.List (foldl1',isPrefixOf)
import Data.Binary
import Control.Monad (replicateM,liftM,forM_)
import System.Environment (getArgs)
import System.Posix.Directory

isFair n = show n == reverse (show n)

square :: Integer -> Integer
square n = n * n

isFairAndSquareR n = isFair n && isFair (square n)

main = do
  args <- getArgs
  if null args
    then do
      -- step 2: count how many within bounds
      s <- liftM (map square . concat) (mapM decodeFile =<< getFASFiles)
      t <- readLn
      forM_ [1..t] $ \i -> do
        [a,b] <- liftM (map read . words) getLine
        putStrLn $ "Case #" ++ show i ++ ": " ++
                   (show $ length $ filter (>= a) $ filter (<= b) s)
    else do
      -- step 1: precompute fair and squares
      let n = read (head args)
      encodeFile ("C-l" ++ show n) $ filter isFairAndSquareR (candidates n)

-- Conjecture: fair and squares numbers are squares of numbers of the form:
-- 1[01]*[012]?[01]*1, and 20*[01]*0*2
-- To stay pessimistic enough, I check [12][01]*[012]?[01]*[12].
-- (it's only twice as many, no point in taking the chance)
candidates 1 = [1,2,3]
candidates l = do
  let l' = (l - 2) `div` 2
  h <- [[1], [2]]
  t <- replicateM l' [0,1]
  m <- if even l then [[]] else [[0],[1],[2]]
  let n = foldl1' (\a b -> 10*a + b) (h ++ t ++ m ++ reverse t ++ h)
  return n

getFASFiles = do
  s <- openDirStream "."
  fs <- takeWhileM (not . null) (repeat $ readDirStream s)
  closeDirStream s
  return (filter (isPrefixOf "C-l") fs)

takeWhileM p (m:ms) = do
  r <- m
  if p r
    then liftM (r:) (takeWhileM p ms)
    else return []

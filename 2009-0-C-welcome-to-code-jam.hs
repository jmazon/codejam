import Data.Array.Unboxed
import Data.List
import Control.Monad
import System.IO

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    s <- getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++  present (solve s)

present n = replicate (4 - length s) '0' ++ s where s = show n
solve s = (foldl' f (listArray (bounds ref) (1:repeat 0) :: UArray Int Int) s)
          ! snd (bounds ref)
    where f a c = listArray (bounds ref)
                  [ ((a!i) + if c == ref!i then a!(i-1) else 0) `mod` 10000
                    | i <- indices ref ]
ref :: UArray Int Char
ref = listArray (0,length s - 1) s where s = "_welcome to code jam"

-- SUCCESS

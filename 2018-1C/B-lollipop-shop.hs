import Data.Ord
import Data.Array
import Data.List
import Control.Monad
import System.IO
import System.Exit

main = do
  hSetBuffering stdout LineBuffering
  t <- readLn
  replicateM_ t $ do
    n <- readLn
    loop n (listArray (0,n-1) (repeat True)) (listArray (0,n-1) (repeat 0))

loop 0 _ _ = return ()
loop n stock stats = do
  (d:wanted) <- map read . words <$> getLine
  when (d < 0) exitFailure
  let stats' = accum (+) stats $ map (\c -> (c,1)) wanted
      accepts = filter ((`elem` wanted)) $
                map fst $
                sortBy (comparing snd) $
                filter ((stock!) . fst) $
                assocs stats'
      (choice,stock') | null accepts = (-1,stock)
                      | otherwise = (head accepts,stock // [(choice,False)])
  print choice
  loop (n-1) stock' stats'

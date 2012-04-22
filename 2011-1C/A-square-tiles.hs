import Data.Array
import Data.List
import Control.Monad

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [r,c] <- liftM (map read . words) getLine
    g <- replicateM r (liftM (listArray (1,c)) getLine)
    putStrLn $ "Case #" ++ show i ++ ":"
    case mapAccumL solve (Just []) g of
      (Just [],g') -> mapM_ (putStrLn . elems) g'
      _ -> putStrLn "Impossible"

solve Nothing x = (Nothing,x)
solve (Just l) x = case r of
                     Just (l',x') -> (Just l',x')
                     Nothing -> (Nothing,x)
    where r = do
            forM_ l $ \i -> guard (x!i == '#' && x!(i+1) == '#')
            let x' = x // [ (i,'\\') | i <- l ] // [ (i+1,'/') | i <- l ]
            let is = map fst $ filter ((=='#') . snd) $ assocs x'
            l' <- grouped is
            let x'' = x' // [ (i,'/') | i <- l' ] // [ (i+1,'\\') | i <- l' ]
            return (l',x'')

grouped [] = return []
grouped (i:j:is) | j==i+1 = liftM (i:) (grouped is)
grouped _ = Nothing

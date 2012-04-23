import Data.Array
import Data.Bits (testBit)
import Data.List (foldr,unfoldr,group)
import Data.FingerTree.PSQueue (Binding(..))
import qualified Data.FingerTree.PSQueue as PSQ
import Control.Monad
import Control.Arrow
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  t <- readLn
  forM_ [1..t] $ \i -> do
    [m,n] <- fmap (map read . words) getLine
    b <- liftM
         (listArray ((0,0),(m-1,n-1)) . concat)
         (replicateM m (liftM (for [n-1,n-2..0] . testBit .
                               (read::String->Integer) . ("0x"++))
                        getLine))
    let s = listArray (bounds b)
            [ if i<1 || j<1 || 
                 c==b!(i-1,j) || c==b!(i,j-1) || c/=b!(i-1,j-1)
              then 1
              else 1 + min (min (s!(i-1,j)) (s!(i,j-1))) (s!(i-1,j-1))
              | (i,j) <- indices b, let c = b!(i,j) ]
        h = PSQ.fromList $ map (uncurry (:->) . second negate) (assocs s)
        r = map (head &&& length) $ group $ unfoldr cut (h)
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (length r)
    mapM_ (putStrLn . \(i,j) -> show i ++ " " ++ show j) r
for = flip map

cut (h) | PSQ.null h  = Nothing
        | otherwise = Just (-s1,h')
    where Just ((i1,j1) :-> s1) = PSQ.findMin h
          h' = foldr (PSQ.updateWithKey u) h
               [(i,j)|i<-[i1+s1+1..i1-s1],j<-[j1+s1+1..j1-s1]]
          u (i,j) s | i<=i1 && j<=j1 = Nothing
                    | otherwise      = Just (max (min (i1-i) (j1-j)) s)

-- SUCCESS (at LAST!)
-- TBD: measure performance change between foldl' and foldr

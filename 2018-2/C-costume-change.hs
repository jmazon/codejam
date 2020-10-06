import Data.Ix
import Control.Monad
import Control.Arrow (second)
import qualified Data.ByteString.Char8 as B

-- for Data.Graph.MaxBipartiteMatching
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List ( foldl' )

readInt :: B.ByteString -> Int
readInt b = i where Just (i,_) = B.readInt b

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readInt <$> B.getLine
    answer <- (n^2 -) . sum . map (M.size . matching) . M.elems .
              M.fromListWith S.union . map (second S.singleton) .
              flip zip (range ((1,1),(n,n))) . concat <$>
              replicateM n (B.words <$> B.getLine)
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

-- Next two functions are Data.Graph.MaxBipartiteMatching
-- from hackage's maxBipartiteMatching-0.1.0.0

matching :: (Ord a, Ord b) => S.Set (a,b) -> M.Map b a
matching g = opt (M.keys fwd, []) fwd M.empty where
  fwd = foldl' (\m (x,y) -> M.insertWith (++) x [y] m) M.empty (S.toList g)

opt :: (Ord a, Ord b) => ([a],[a]) -> M.Map a [b] -> M.Map b a -> M.Map b a
opt (x:free,failed) fwd mat =
    either (flip (opt (free,x:failed)) mat) (opt (free++failed,[]) fwd) $
    right fwd [] x
  where right rm path x' =
            maybe (Left rm) (left $ M.delete x' rm) $ M.lookup x' rm
          where
            left rm' [] = Left rm'
            left rm' (y:ys) = maybe
                (Right $ foldr (uncurry $ flip M.insert) mat path') --A
                (either (flip left ys) Right . right rm' path')     --B
                (M.lookup y mat)                                    --C
              where
                path' = (x',y) : path
opt ([],_) _ mat = mat

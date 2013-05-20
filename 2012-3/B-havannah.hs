import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.List ((\\),intercalate,groupBy)
import Data.UnionFind.ST
import Data.Map ((!),insert,empty,assocs)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.ST
import Control.Arrow
neighbors (x,y) = [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y),(x-1,y-1),(x,y-1)]
ifte t e p = if p then t else e
pairs (x:xs) = map ((,) x) xs ++ pairs xs
pairs _ = []
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    [s,m] <- liftM (map read . words) getLine
    ms <- replicateM m $ liftM (((!!0) &&& (!!1)) . map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ solve s ms
solve s ms = runST $ do
  places <- foldM (\m p -> fresh p >>= \v -> return (insert p v m)) empty ms
  let corners = catMaybes $ map (flip M.lookup places)
                                [ (1,1), (1,s), (s,2*s-1)
                                , (2*s-1,2*s-1), (2*s-1,s), (s,1) ]
  let edges = map ((\\ corners) . catMaybes . flip map (assocs places) .
                   (\f (k,v) -> guard (f k) >> return v))
              [ (== 1) . fst , (== (1-s)) . uncurry (-) , (== (2*s-1)) . snd
              , (== (2*s-1)) . fst , (== (s-1)) . uncurry (-) , (== 1) . snd ]
  let go _ _ [] = return "none"
      go t s (m:ms) = do
        let ns = map ( guard . (`S.member` s) &&& return . (places!) >>>
                       uncurry (>>) ) $ neighbors m
            p = places ! m
            checkPair (n1,n2) = (>= 2) $ length $ filter (elem Nothing) $
                                groupBy ((==) `on` (== Just True)) $
                                take 6 $ dropWhile (/= Just True) $ cycle $
                                map (fmap (`elem` [n1,n2])) ns
        ring <- liftM (ifte ["ring"] [] . or . map checkPair) $
                filterM (uncurry equivalent) $ pairs (catMaybes ns)
        mapM_ (union p) (catMaybes ns)
        bridge <- liftM (ifte ["bridge"] [] . (>= 2) . length) $
                  filterM (equivalent p) corners
        fork <- liftM (ifte ["fork"] [] . (>= 3) . length . filter (not.null)) $
                mapM (filterM (equivalent p)) edges
        case bridge ++ fork ++ ring of
          [] -> go (t+1) (S.insert m s) ms
          r  -> return (intercalate "-" r ++ " in move " ++ show t)
  go 1 S.empty ms

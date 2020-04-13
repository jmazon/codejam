{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.Monoid

import qualified Data.Set as S
import Data.Array
import Data.Array.ST

data Pos = Pos !Int !Int deriving (Eq,Ord,Ix)
data Dancer = Dancer { dSkill :: !Int
                     , dUp    :: !(Maybe Pos)
                     , dDown  :: !(Maybe Pos)
                     , dLeft  :: !(Maybe Pos)
                     , dRight :: !(Maybe Pos) }

setUp,setDown,setLeft,setRight :: Maybe Pos -> Dancer -> Dancer
setUp    p d = d { dUp    = p }
setDown  p d = d { dDown  = p }
setLeft  p d = d { dLeft  = p }
setRight p d = d { dRight = p }

competition :: Array Pos Dancer -> Int
competition g0 = runST $ do
  grid <- thaw (Just <$> g0) :: ST s (STArray s Pos (Maybe Dancer))

  -- | One competition round.  Returns skill sum of eliminated
  -- competitors and list of their compass neighbors.
  let dance = fmap end . foldlMapM' eliminate <=<
              filterM (eliminated <=< readArray grid)
      end (0,_) = Nothing
      end (v,f) = Just (getSum v,S.toList f)

      eliminated Nothing = pure False -- if both fringe and eliminated last round
      eliminated (Just d) = do
        neighbors <- catMaybes <$> mapM (fmap join . traverse (readArray grid))
                                        [dUp d,dDown d,dLeft d,dRight d]
        pure (length neighbors * dSkill d < sum (map dSkill neighbors))

      eliminate p = readArray grid p >>= \(Just d) -> do
        ns <- fmap catMaybes $ forM [ (dUp,   setDown, dDown)
                                    , (dDown, setUp,   dUp)
                                    , (dLeft, setRight,dRight)
                                    , (dRight,setLeft, dLeft) ] $
          \(neighbor,set,get) -> forM (neighbor d) $ \p' -> do
            writeArray grid p' . fmap (set (get d)) =<< readArray grid p'
            pure p'
        writeArray grid p Nothing
        pure (Sum (dSkill d),S.fromList ns)
        
  -- scanl maintains the list of the dancefloor's successive skill sums
  sum . scanl (-) (sum (dSkill <$> g0)) <$> unfoldrM' dance (indices g0)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \tn -> do
    [r,c] <- map read . words <$> getLine
    start <- replicateM r (map read . words <$> getLine)
    let grid = listArray (Pos 1 1,Pos r c) $ concat $ zipWith row [1..r] start
        row i l = zipWith (col i) [1..c] l
        col i j n = Dancer { dSkill = n
                           , dUp    = guard (i > 1) *> Just (Pos (i-1) j)
                           , dDown  = guard (i < r) *> Just (Pos (i+1) j)
                           , dLeft  = guard (j > 1) *> Just (Pos  i (j-1))
                           , dRight = guard (j < c) *> Just (Pos  i (j+1)) }
    putStrLn $ "Case #" ++ show tn ++ ": " ++ show (competition grid)

unfoldrM' :: (Monad m,MonadPlus f) => (a -> m (Maybe (b,a))) -> a -> m (f b)
unfoldrM' f = go where
  go z = f z >>= \case Nothing     -> pure mzero
                       Just (x,z') -> fmap (mplus (pure x)) (go z')

foldlMapM' :: (Foldable f,Monoid b,Monad m) => (a -> m b) -> f a -> m b
foldlMapM' f xs = foldr f' pure xs mempty where
  f' x k bl = f x >>= \br -> k $! mappend bl br

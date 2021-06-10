{-# LANGUAGE LambdaCase #-}

import Control.Monad (forM_,replicateM,guard,forever,void)
import Control.Monad.Cont (runContT,callCC,lift)
import Control.Monad.ST (runST)
import Data.Bifunctor
import Data.Bool
import Data.Function (fix)
import Data.List (partition,find)
import Data.Map.Strict ((!))
import Data.Maybe (mapMaybe)
import Data.STRef
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [r,_c,f,s] <- map read . words <$> getLine
    src <- replicateM r getLine
    dst <- replicateM r getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve f s src dst)

solve :: Int -> Int -> [String] -> [String] -> Int
solve f s src dst = cost
  where
    delta :: [[Bool]]
    delta = (zipWith . zipWith) (/=) src dst

    gs,ms :: Set (Int,Int)
    (gs,ms) = bimap (Set.fromList . map fst) (Set.fromList . map fst) $
              partition ((== 'G') . snd) $ concat $
              zipWith3 (\i srcRow dstRow ->
                          mapMaybe (\(j,se,de) -> ((i,j),se) <$ guard de)
                            (zip3 [0..] srcRow dstRow))
                [0..] src delta

    (cost,_) = munkresI (Set.disjointUnion ms gs) (Set.disjointUnion gs ms) $
               curry $ \ case (Right g,Right m) -> min (dist g m * s) (2*f)
                              (Left _,Left _) -> 0
                              _ -> f

dist :: (Int,Int) -> (Int,Int) -> Int
dist (a,b) (c,d) = abs (c-a) + abs (d-b)      

data Step = One | Two | Three | Four | Five | Six | Seven
data Mask = None | Star | Prime deriving Eq

-- Adapted from Robert Pilgrim, “Tutorial on Implementation of
-- Munkres' Assignment Algorithm”
munkresI :: (Ord a,Ord b) => Set a -> Set b -> (a -> b -> Int) -> (Int,Set (a,b))
munkresI s t c = runST topLevel
  where
    topLevel = do
      costs <- newSTRef Map.empty
      mask <- newSTRef (Map.fromSet (const None) (Set.cartesianProduct s t))
      rowCover <- newSTRef (Map.fromSet (const False) s)
      colCover <- newSTRef (Map.fromSet (const False) t)
      pathRow0 <- newSTRef undefined
      pathCol0 <- newSTRef undefined
      step <- newSTRef One
      flip runContT pure $ callCC $ \done -> forever $ do
        lift (readSTRef step) >>= \ case
          One -> lift $ stepOne step costs
          Two -> lift $ stepTwo step costs mask rowCover colCover
          Three -> lift $ stepThree step mask colCover
          Four -> lift $ stepFour step costs mask rowCover colCover pathRow0 pathCol0
          Five -> lift $ stepFive step mask rowCover colCover pathRow0 pathCol0
          Six -> lift $ stepSix step costs rowCover colCover
          Seven -> lift (stepSeven costs mask) >>= done

    -- | For each row of the cost matrix, find the smallest element
    -- and subtract it from every element in its row.  When finished,
    -- go to step 2.
    stepOne step costs = do
      forM_ (Set.elems s) $ \a -> do
        let rowMin = minimum [ c a b | b <- Set.elems t ]
        forM_ (Set.elems t) $ \b ->
          modifySTRef' costs (Map.insert (a,b) (c a b - rowMin))
      writeSTRef step Two

    -- | Find a zero (Z) in the resulting matrix.  If there is no
    -- starred zero in its row or column, star Z.  Repeat for each
    -- element in the matrix.  Go to step 3.
    stepTwo step costs mask rowCover colCover = do
      forM_ (Set.elems s) $ \a ->
        forM_ (Set.elems t) $ \b ->
        whenM (readSTRef costs >>= \cs -> pure (cs!(a,b) == 0)) $
        whenM (readSTRef rowCover >>= \rc -> pure (rc!a == False)) $
        whenM (readSTRef colCover >>= \cc -> pure (cc!b == False)) $ do
          modifySTRef' mask (Map.insert (a,b) Star)
          modifySTRef' rowCover (Map.insert a True)
          modifySTRef' colCover (Map.insert b True)
      forM_ (Set.elems s) $ \a -> modifySTRef' rowCover (Map.insert a False)
      forM_ (Set.elems t) $ \b -> modifySTRef' colCover (Map.insert b False)
      writeSTRef step Three

    -- | Cover each column containing a starred zero.  If K columns
    -- are covered, the starred zeros describe a complete set of
    -- unique assignments.  In this case, go to DONE, otherwise, go to
    -- step 4.
    stepThree step mask colCover = do
      forM_ (Set.elems s) $ \a ->
        forM_ (Set.elems t) $ \b ->
        whenM (readSTRef mask >>= \m -> pure (m!(a,b) == Star)) $
        modifySTRef' colCover (Map.insert b True)
      colCount <- Map.size . Map.filter id <$> readSTRef colCover
      writeSTRef step $ if colCount >= min (Set.size t) (Set.size s)
                        then Seven
                        else Four

    -- | Find a noncovered zero and prime it.  If there is no starred
    -- zero in the row conraining this primed zero, go to step 5.
    -- Otherwise, cover this row and uncover the column containing the
    -- starred zero.  Continue in this manner until there are no
    -- uncovered zeros left.  Save the smallest uncovered value and go
    -- to step 6.
    stepFour step costs mask rowCover colCover pathRow0 pathCol0 = fix $ \loop -> do
      findAZero costs rowCover colCover >>= \case
        Nothing -> writeSTRef step Six
        Just (a,b) -> do
          modifySTRef' mask (Map.insert (a,b) Prime)
          starInRow mask a >>= \case
            True -> do
              b' <- findStarInRow mask a
              modifySTRef' rowCover (Map.insert a True)
              modifySTRef' colCover (Map.insert b' False)
              loop
            False -> do
              writeSTRef step Five
              writeSTRef pathRow0 a
              writeSTRef pathCol0 b

    -- methods to support step 4
    findAZero costs rowCover colCover = do
      rc <- readSTRef rowCover
      cc <- readSTRef colCover
      fmap fst . find (\((a,b),v) -> v == 0 && rc!a == False && cc!b == False) . Map.assocs <$> readSTRef costs
    starInRow mask a = do
      m <- readSTRef mask
      pure $ any (\b -> m!(a,b) == Star) (Set.elems t)
    findStarInRow mask a = do
      m <- readSTRef mask
      pure $ head $ filter (\b -> m!(a,b) == Star) (Set.elems t)

    -- | Construct a series of alternating primed and starred zeros as
    -- follows.  Let Z0 represent the uncovered primed zeros found in
    -- step 4.  Let Z1 denote the starred zero in the column of Z0 (if
    -- any).  Let Z2 denote the primed zero in the row of Z1 (there
    -- will always be one).  Continue until the series terminates at a
    -- primed zero that has no starred zero in its column.  Unstar
    -- each starred zero of the zeries, star each primed zero of the
    -- series, erase all primes and uncover every line in the
    -- matrix. Return to step 3.
    stepFive step mask rowCover colCover pathRow0 pathCol0 = do
      pr0 <- readSTRef pathRow0
      pc0 <- readSTRef pathCol0
      flip fix [(pr0,pc0)] $ \loop path@((_,b):_) -> do
        findStarInCol mask b >>= \case
          Just a -> do
            let path' = (a,b) : path
            b' <- findPrimeInRow mask a
            let path'' = (a,b') : path'
            loop path''
          Nothing -> do
            augmentPath mask path
            clearCovers rowCover colCover
            erasePrimes mask
            writeSTRef step Three

    -- methods to support step 5
    findStarInCol mask b = do
      m <- readSTRef mask
      pure $ find (\a -> m!(a,b) == Star) (Set.elems s)
    findPrimeInRow mask a = do
      m <- readSTRef mask
      pure $ head $ filter (\b -> m!(a,b) == Prime) (Set.elems t)
    augmentPath mask path = do
      forM_ path $ \p ->
        modifySTRef mask $ flip Map.adjust p $ \case
          Star -> None
          _ -> Star
    clearCovers rowCover colCover = do
      modifySTRef' rowCover (fmap (const False))
      modifySTRef' colCover (fmap (const False))
    erasePrimes mask = do
      modifySTRef' mask $ fmap $ \case
        Prime -> None
        x -> x

    -- | Add the value found in step 4 to every element of each
    -- covered row, and subtract it from every element of each
    -- uncovered column.  Return to step 4 without altering any stars,
    -- primes, or covered lines.
    stepSix step costs rowCover colCover = do
      minVal <- findSmallest costs rowCover colCover
      rc <- readSTRef rowCover
      cc <- readSTRef colCover
      modifySTRef' costs $ Map.mapWithKey $ \(a,b) v ->
        v
        + (if rc!a == True then minVal else 0)
        - (if cc!b == False then minVal else 0)
      writeSTRef step Four

    -- method to support step 6
    findSmallest costs rowCover colCover = do
      rc <- readSTRef rowCover
      cc <- readSTRef colCover
      minimum . Map.filterWithKey (\(a,b) _ -> rc!a == False && cc!b == False) <$> readSTRef costs

    -- | Assignment pairs are indicated by the positions of the
    -- starred zeros in the cost matrix.
    stepSeven costs mask = do
      m <- readSTRef mask
      vas <- Map.mapMaybeWithKey (\k v -> if v == 0 && m!k == Star
                                  then Just (uncurry c k)
                                  else Nothing) <$>
            readSTRef costs
      pure (sum vas,Map.keysSet vas)

whenM :: Monad m => m Bool -> m a -> m ()
whenM p m = p >>= bool (pure ()) (void m)

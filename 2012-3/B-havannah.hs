import Data.STRef

import Data.Bool
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.List ((\\),intercalate,groupBy)
import Data.Map ((!),insert,empty,assocs)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.ST
import Control.Arrow

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y),(x-1,y-1),(x,y-1)]

ifte :: a -> a -> Bool -> a
ifte = flip bool

pairs :: [a] -> [(a,a)]
pairs (x:xs) = map ((,) x) xs ++ pairs xs
pairs _ = []

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [s,m] <- liftM (map read . words) getLine
    ms <- replicateM m $ liftM (((!!0) &&& (!!1)) . map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ solve s ms

solve :: Int -> [(Int,Int)] -> [Char]
solve s0 ms0 = runST $ do
  places <- foldM (\m p -> fresh p >>= \v -> return (insert p v m)) empty ms0
  let corners = catMaybes $ map (flip M.lookup places)
                                [ (1,1), (1,s0), (s0,2*s0-1)
                                , (2*s0-1,2*s0-1), (2*s0-1,s0), (s0,1) ]
  let edges = map ((\\ corners) . catMaybes . flip map (assocs places) .
                   (\f (k,v) -> guard (f k) >> return v))
              [ (== 1) . fst , (== (1-s0)) . uncurry (-) , (== (2*s0-1)) . snd
              , (== (2*s0-1)) . fst , (== (s0-1)) . uncurry (-) , (== 1) . snd ]
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
  go (1 :: Int) S.empty ms0

------------------------------------------------------------
-- Data.UnionFind.ST

-- | An implementation of Tarjan's UNION-FIND algorithm.  (Robert E
-- Tarjan. \"Efficiency of a Good But Not Linear Set Union Algorithm\", JACM
-- 22(2), 1975)
--
-- The algorithm implements three operations efficiently (all amortised
-- @O(1)@):
--
--  1. Check whether two elements are in the same equivalence class.
--
--  2. Create a union of two equivalence classes.
--
--  3. Look up the descriptor of the equivalence class.
-- 
-- The implementation is based on mutable references.  Each
-- equivalence class has exactly one member that serves as its
-- representative element.  Every element either is the representative
-- element of its equivalence class or points to another element in
-- the same equivalence class.  Equivalence testing thus consists of
-- following the pointers to the representative elements and then
-- comparing these for identity.
--
-- The algorithm performs lazy path compression.  That is, whenever we
-- walk along a path greater than length 1 we automatically update the
-- pointers along the path to directly point to the representative
-- element.  Consequently future lookups will be have a path length of
-- at most 1.

-- | The abstract type of an element of the sets we work on.  It is
-- parameterised over the type of the descriptor.
newtype Point s a = Pt (STRef s (Link s a)) deriving Eq

data Link s a
    = Info {-# UNPACK #-} !(STRef s (Info a))
      -- ^ This is the descriptive element of the equivalence class.
    | Link {-# UNPACK #-} !(Point s a)
      -- ^ Pointer to some other element of the equivalence class.
     deriving Eq

data Info a = MkInfo
  { weight :: {-# UNPACK #-} !Int
    -- ^ The size of the equivalence class, used by 'union'.
  , descr  :: a
  } deriving Eq

-- | /O(1)/. Create a fresh point and return it.  A fresh point is in
-- the equivalence class that contains only itself.
fresh :: a -> ST s (Point s a)
fresh desc = do
  info <- newSTRef (MkInfo { weight = 1, descr = desc })
  l <- newSTRef (Info info)
  return (Pt l)

-- | /O(1)/. @repr point@ returns the representative point of
-- @point@'s equivalence class.
--
-- This method performs the path compresssion.
repr :: Point s a -> ST s (Point s a)
repr point@(Pt l) = do
  link <- readSTRef l
  case link of
    Info _ -> return point
    Link pt'@(Pt l') -> do
      pt'' <- repr pt'
      when (pt'' /= pt') $ do
        -- At this point we know that @pt'@ is not the representative
        -- element of @point@'s equivalent class.  Therefore @pt'@'s
        -- link must be of the form @Link r@.  We write this same
        -- value into @point@'s link reference and thereby perform
        -- path compression.
        link' <- readSTRef l'
        writeSTRef l link'
      return pt''

-- | Return the reference to the point's equivalence class's
-- descriptor.
descrRef :: Point s a -> ST s (STRef s (Info a))
descrRef point@(Pt link_ref) = do
  link <- readSTRef link_ref
  case link of
    Info info -> return info
    Link (Pt link'_ref) -> do
      link' <- readSTRef link'_ref
      case link' of
        Info info -> return info
        _ -> descrRef =<< repr point

-- | /O(1)/. Return the descriptor associated with argument point's
-- equivalence class.
descriptor :: Point s a -> ST s a
descriptor point = do
  descr <$> (readSTRef =<< descrRef point)

-- | /O(1)/. Replace the descriptor of the point's equivalence class
-- with the second argument.
setDescriptor :: Point s a -> a -> ST s ()
setDescriptor point new_descr = do
  r <- descrRef point
  modifySTRef r $ \i -> i { descr = new_descr }

modifyDescriptor :: Point s a -> (a -> a) -> ST s ()
modifyDescriptor point f = do
  r <- descrRef point
  modifySTRef r $ \i -> i { descr = f (descr i) }

-- | /O(1)/. Join the equivalence classes of the points (which must be
-- distinct).  The resulting equivalence class will get the descriptor
-- of the second argument.
union :: Point s a -> Point s a -> ST s ()
union p1 p2 = union' p1 p2 (\_ d2 -> return d2)

-- | Like 'union', but sets the descriptor returned from the callback.
-- 
-- The intention is to keep the descriptor of the second argument to
-- the callback, but the callback might adjust the information of the
-- descriptor or perform side effects.
union' :: Point s a -> Point s a -> (a -> a -> ST s a) -> ST s ()
union' p1 p2 update = do
  point1@(Pt link_ref1) <- repr p1
  point2@(Pt link_ref2) <- repr p2
  -- The precondition ensures that we don't create cyclic structures.
  when (point1 /= point2) $ do
    Info info_ref1 <- readSTRef link_ref1
    Info info_ref2 <- readSTRef link_ref2
    MkInfo w1 d1 <- readSTRef info_ref1 -- d1 is discarded
    MkInfo w2 d2 <- readSTRef info_ref2
    d2' <- update d1 d2
    -- Make the smaller tree a a subtree of the bigger one.  The idea
    -- is this: We increase the path length of one set by one.
    -- Assuming all elements are accessed equally often, this means
    -- the penalty is smaller if we do it for the smaller set of the
    -- two.
    if w1 >= w2 then do
      writeSTRef link_ref2 (Link point1)
      writeSTRef info_ref1 (MkInfo (w1 + w2) d2')
     else do
      writeSTRef link_ref1 (Link point2)
      writeSTRef info_ref2 (MkInfo (w1 + w2) d2')

-- | /O(1)/. Return @True@ if both points belong to the same
-- | equivalence class.
equivalent :: Point s a -> Point s a -> ST s Bool
equivalent p1 p2 = (==) <$> repr p1 <*> repr p2

-- | /O(1)/. Returns @True@ for all but one element of an equivalence
-- class.  That is, if @ps = [p1, .., pn]@ are all in the same
-- equivalence class, then the following assertion holds.
-- 
-- > do rs <- mapM redundant ps
-- >    assert (length (filter (==False) rs) == 1)
-- 
-- It is unspecified for which element function returns @False@, so be
-- really careful when using this.
redundant :: Point s a -> ST s Bool
redundant (Pt link_r) = do
  link <- readSTRef link_r
  case link of
    Info _ -> return False
    Link _ -> return True

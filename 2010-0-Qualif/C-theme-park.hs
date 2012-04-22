import Control.Monad.Reader
import Data.Array
import Data.Functor
import Data.Maybe
import qualified Data.IntMap as M

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \tt -> do
    [r, k, n] <- readIntLine
    gs <- listArray (0,n-1) <$> readIntLine
    putStrLn $ "Case #" ++ show tt ++ ": " ++ show (dayIncome r k n gs)

readIntLine = map read . words <$> getLine

type Group = Int     -- actually an index
type GroupSize = Int -- enough in both small and large sets
type GroupList = Array Group GroupSize

data Coaster = Coaster { run        :: !Int
                       , firstGroup :: !Group
                       , income     :: !Integer
                       , roomLeft   :: !GroupSize }

data Park = Park { runs       :: Int
                 , capacity   :: GroupSize
                 , groupCount :: Group
                 , groups     :: GroupList }

-- a single test case: compute the income for a day
dayIncome r k n gs =
    dayIncome' M.empty r True (Coaster 0 0 0 0) (Park r k n gs)

-- helper: remember where the line stops between two turns
dayIncome' a r p c =
  if run c == r                  -- reached end of day?
  then return (income c)         -- victory!
  else do
    c' <- boardCoaster c
    let !a' = if p then M.insert (firstGroup c) c a else a
        l   = M.lookup (firstGroup c') a'
    if p && isJust l                           -- have a period?
      then let c0 = fromJust l                   -- get start of period
               period = run c' - run c0          -- compute its length
               payroll = income c' - income c0   -- its gains
               q = (r - run c') `div` period     -- its count
           in dayIncome' a' r False
                         c' { run = run c' + q * period
                            , income = income c'
                                       + fromIntegral q * payroll }
      else dayIncome' a' r p c'

-- hoard in as many groups as can fit in a coaster run
boardCoaster c = do
  r <- capacity <$> ask
  c' <- boardCoaster' c { roomLeft = r } (firstGroup c)
  return c' { run = run c + 1 }

-- helper: remember where the line started and keep track of remaining space
boardCoaster' c f0 = do
  c' <- boardGroup c
  r0 <- capacity <$> ask
  if roomLeft c' < 0 ||                        -- coaster full
     (firstGroup c == f0 && roomLeft c /= r0)  -- queue exhausted
    then return c
    else boardCoaster' c' f0

-- adjust internal structure to consider one whole group got in
boardGroup (Coaster n f i r) = do
  s <- groupSize f
  f'<- nextGroup f
  return $ Coaster n f' (i + fromIntegral s) (r - s)

groupSize i = ask >>= return . (!i) . groups
nextGroup i = ask >>= return . mod (succ i) . groupCount

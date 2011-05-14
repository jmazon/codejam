import Data.Char
import Data.Maybe
import Data.Array
import Data.List
import Control.DeepSeq
import Control.Arrow
import Control.Monad
import qualified Data.Map as M
import System.IO

type Square = Array (Int,Int) Char
type State = Array (Int,Int) (M.Map Int String)

(a,b) +: (c,d) = (a+c,b+d)

main = do
  hSetBuffering stdout LineBuffering
  t <- readLn
  forM_ [1..t] $ \i -> do
    [w,q] <- fmap (map read . words) getLine
    sq <- fmap (listArray ((1,1),(w,w)) . concat) (replicateM w getLine)
    qs <- fmap (map read . words) getLine :: IO [Int]
    let s0 = initState sq
    putStrLn $ "Case #" ++ show i ++ ":"
    forM_ qs $ putStrLn . fromJust . flip M.lookup (solve sq qs s0 M.empty)

initState :: Square -> State
initState s = accumArray (flip const) M.empty (bounds s) $
              map (\(p,v) -> (p,M.singleton (ord v-ord '0') (v:""))) $
              filter (isDigit . snd) $
              assocs s

transition :: Square -> ((Int,Int),(Int,String)) -> [((Int,Int),(Int,String))]
transition s (o1,(v,e)) = do
  op <- map (o1+:) [(0,1),(0,-1),(1,0),(-1,0)]
  guard (inRange (bounds s) op)
  o2 <- map (op+:) [(0,1),(0,-1),(1,0),(-1,0)]
  guard (inRange (bounds s) o2)
  let r = ( o2
          , ( if s!op == '+'
              then v + ord (s!o2) - ord '0'
              else v - ord (s!o2) - ord '0'
            , e ++ (s!op : s!o2 : "") ) )
  deepseq r (return r)

solve :: Square -> [Int] -> State -> M.Map Int String -> M.Map Int String
solve _ [] _ as = as
solve sq qs s as = deepseq s' $ solve sq qs' s' as'
    where as' = M.union as $
                M.fromList $
                filter ((`elem` qs) . fst) $
                concatMap M.assocs $
                elems s
          qs' = qs \\ M.keys as'
          s' = accumArray (\m (v,e) -> M.insertWith min v e m) M.empty (bounds s) $
               concatMap (transition sq) $
               concatMap (\(p,m) ->
                          map ((,) p) $
                          M.assocs m) $
               assocs s

g1 = listArray ((1,1),(5,5)) "2+1-2+3-4+5+2+1-4-0-9+5+1"

-- still not good

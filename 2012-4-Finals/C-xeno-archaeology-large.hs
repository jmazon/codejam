import Data.Function (on)
import Data.List (unfoldr,minimumBy,foldl1')
import Data.Monoid ((<>))
import Control.Monad (zipWithM,ap,guard,liftM2,join)
data Rect = Rect !Int !Int !Int !Int
world = Rect wMin wMax wMin wMax
wMax = 10^17; wMin = -wMax
main = zipWithM format [1..] . map solve . parse . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++
                        maybe "Too damaged" show' j
    where show' (x,y) = show x ++ " " ++ show y
solve ts | null rs   = Nothing
         | otherwise = Just (minimumBy order rs)
    where rs = join (solve' ts `fmap` [True,False] `ap` [True,False])
solve' ts px py = do
  let constraint ((x,y),c)
       | parity px x == parity py y = guard (parity px x /= c) >> return [world]
       | parity px x /= c           = return [ Rect (x+y) wMax (x-y) wMax
                                             , Rect wMin (x+y) wMin (x-y) ]
       | otherwise                  = return [ Rect (x+y) wMax wMin (x-y)
                                             , Rect wMin (x+y) (x-y) wMax ]
  concatMap (neighborhood px py) $ join $
    map (foldl1' intersections) $ mapM constraint ts
parity p n = p == even n
intersections = (join .) . liftM2 intersection
intersection (Rect a1 a2 b1 b2) (Rect x1 x2 y1 y2) =
  guard (ax1 <= ax2 && by1 <= by2) >> return (Rect ax1 ax2 by1 by2)
  where ax1 = max a1 x1; ax2 = min a2 x2; by1 = max b1 y1; by2 = min b2 y2
neighborhood px py (Rect a b c d) = concatMap near ps
    where ps = [(0,0),(a,0),(0,a),(b,0),(0,b),(c,0),(0,-c),(d,0),(0,-d)] ++
               diagInter `fmap` [a,b] `ap` [c,d]
          diagInter s d = ((s+d) `quot` 2,(s-d) `quot` 2)
          near (x0,y0) = [ (x,y) | dx <- [-3 .. 3], dy <- [-3 .. 3],
                                   abs dx + abs dy <= 3,
                                   let x = x0 + dx, parity px x,
                                   let y = y0 + dy, parity py y,
                                   x + y >= a, x + y <= b,
                                   x - y >= c, x - y <= d ]
parse (n:ns) = unfoldr go ts : parse ns'
    where (ts,ns') = splitAt (3 * read n) ns
          go ns = do
            (x:y:c:ns') <- Just ns
            Just (((read x,read y),c == "#"),ns')
manhattan (x,y) = abs x + abs y
order = (compare `on` manhattan) <>
        flip (compare `on` fst)  <> flip (compare `on` snd)

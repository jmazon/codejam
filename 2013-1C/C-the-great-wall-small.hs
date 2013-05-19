import Data.Array
import Data.Maybe (catMaybes)
import Data.List (partition,foldl')
import Control.Monad (forM_,replicateM)
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    attacks <- replicateM n $ fmap (map read . words) getLine
    let wall = accumArray undefined 0 (-400,400) []
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve wall attacks 0)
solve   _    []    c = c
solve wall attacks c = solve wall' attacks' $! c + successes
    where day = head (minimum attacks)
          (dayAttacks,restAttacks) = partition ((<= day) . head) attacks
          daySuccess [_,_,aw,ae,s,_,_,_] =
              minimum [ wall!p | p <- [2*aw..2*ae] ] < s
          successes = length $ filter daySuccess dayAttacks
          raiseWall w [_,_,aw,ae,s,_,_,_] =
              w // [ (p,max s (w!p)) | p <- [2*aw..2*ae] ]
          wall' = foldl' raiseWall wall dayAttacks
          attacks' = catMaybes (map nextAttack dayAttacks) ++ restAttacks
nextAttack (_:1:_)                   = Nothing
nextAttack (d:c:w:e:s:xs@[dd,dp,ds]) = Just (d+dd:c-1:w+dp:e+dp:s+ds:xs)

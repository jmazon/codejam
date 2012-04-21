import Data.List
import Data.Maybe
import qualified Data.Map as M
import Control.Monad
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    (c:ws) <- fmap words getLine
    let (cs,d:ws') = splitAt (read c) ws
        csm = M.fromList $ concatMap (\[a,b,c]->[((a,b),c),((b,a),c)]) cs
        (ds,n:[g]) = splitAt (read d) ws'
        dsm = M.fromListWith (++) $ concatMap (\[a,b]->[(a,[b]),(b,[a])]) ds
        (r,_) = foldl' f ([],M.empty) g
        f ((h:t),b) i | isJust n = ((fromJust n):t,M.adjust pred h b)
                                   where n = M.lookup (i,h) csm
        f (s,b) i = if Just True == ( M.lookup i dsm >>=
                                      return . (>0) . sum .
                                      map (\i' -> M.findWithDefault 0 i' b) )
                    then ([],M.empty)
                    else (i:s,M.insertWith (+) i 1 b)
    putStrLn $ "Case #" ++ show i ++ ": " ++
               "[" ++ intercalate ", " (map (:"") (reverse r)) ++ "]"

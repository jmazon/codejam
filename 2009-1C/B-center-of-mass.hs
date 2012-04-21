import Data.List
import Control.Applicative
import Control.Monad

main = do
  t <- read <$> getLine
  forM_ [1..t] $ \i -> do
    n <- read <$> getLine
    ffs <- replicateM n $ map read . words <$> getLine
    let ff@[x0,y0,z0,vx,vy,vz] = map (/ fromIntegral n) $
                              getZipList $
                              foldl1' (liftA2 (+)) $
                              ZipList <$> ffs
        r = (x0,y0,z0)
        v = (vx,vy,vz)
        v2 = dotP v v
        d = dotP r v
        v' = norm v
        (dmin,tmin) = if d >= 0
                      then (norm r, 0)
                      else (norm (crossP r v) / v', -d/v2)
    putStrLn $ "Case #" ++ show i ++ ": " 
                 ++ show dmin ++ " " ++ show tmin

dotP (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2
crossP (x1,y1,z1) (x2,y2,z2) =
    ( y1*z2 - y2*z1
    , z1*x2 - x1*z2
    , x1*y2 - y1*x2 )
norm v = sqrt $ dotP v v

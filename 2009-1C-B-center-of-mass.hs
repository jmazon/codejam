import Data.Traversable
import Control.Monad
import Control.Applicative
import Prelude hiding ((**))

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    fs <- replicateM n (fmap (map read . words) getLine)
    let (d,t) = solve fs
    putStrLn $ "Case #" ++ show i ++ ": " ++ show d ++ " " ++ show t

solve fs = (d,t)
    where ZipList [x,y,z,vx,vy,vz] = (/ fromIntegral (length fs)) <$>
                                     sum <$> (sequenceA (ZipList <$> fs))
          r = (x,y,z)
          v = (vx,vy,vz)
          t = max 0 (- r**v / sqr v)
          d = norm $ r +:+ t *: v

infixl 8 **
(a,b,c) ** (x,y,z) = a*x + b*y + c*z

sqr a = a ** a
norm = sqrt . sqr

infixl 6 +:+
(a,b,c) +:+ (x,y,z) = (a+x,b+y,c+z)

infixl 7 *:
x *: (a,b,c) = (x*a,x*b,x*c)

-- SUCCESS

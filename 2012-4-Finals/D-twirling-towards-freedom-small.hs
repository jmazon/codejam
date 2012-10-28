import Data.List (unfoldr)
import Control.Monad (zipWithM_,replicateM)
data Complex = !Int :+ !Int -- I want Num but no floating-point
instance Num Complex where
    (a :+ b) + (c :+ d) = (a+c) :+ (b+d)
    (a :+ b) * (c :+ d) = (a*c - b*d) :+ (a*d + b*c)
    negate (a :+ b) = negate a :+ negate b
    fromInteger n = fromInteger n :+ 0
    abs = undefined; signum = undefined
main = zipWithM_ format [1..] . map (uncurry solve) . parse . map read . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ show j
twirl z zs = -(0 :+ negate 1)*z + zs*(1 :+ negate 1)
sqr (x :+ y) = x^2 + y^2
solve m ss = sqrt . fromIntegral . maximum $ map sqr . scanl twirl 0 . take m . cycle =<< replicateM 4 ss
parse (n:m:ns) = (m,unfoldr toPairs ps) : parse ns'
    where (ps,ns') = splitAt (2*n) ns
          toPairs (a:b:xs) = Just (a:+b,xs)
          toPairs _ = Nothing
parse _ = []

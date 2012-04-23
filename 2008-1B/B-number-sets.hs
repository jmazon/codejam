import Data.Equivalence.STT
import Data.Numbers.Primes
import qualified Data.IntSet as S
import Control.Monad
import Control.Monad.Identity
import System.IO
import Control.Monad.ST.Trans

solve (a,b,p) = runIdentity $ runST $ do
  e <- leastEquiv id min
  forM_ (takeWhile (<= b-a) $ dropWhile (<p) primes) $ \p -> do
    let n = (a+p-1) `div` p * p
    equateAll e [n,n+p..b]
  fmap (S.size . S.fromList) (mapM (classDesc e) [a..b])

main = hSetBuffering stdout LineBuffering >> (interact $ unlines . zipWith c [1..] . map solve . parse . map read . tail . words)
c i j = "Case #" ++ show i ++ ": " ++ show j
parse (a:b:p:ns) = (a,b,p) : parse ns
parse _ = []

-- success, but 44 minutes on large, compiled and optimized

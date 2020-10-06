module Main (main) where

import Data.List

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Test.QuickCheck

(^!) :: Num n => n -> Int -> n
(^!) = (^)
infixr 8 ^!

main :: IO ()
main = do
  print $ find (\stock -> math 1 2 stock > bsInt 1 2 stock) [2251799831515689..10^!18]
  let readInt b = i where Just (i,_) = B.readInt b
  t <- readInt <$> B.getLine
  forM_ [1..t] $ \testNumber -> do
    [l,r] <- map readInt . B.words <$> B.getLine
    let solver = math
        (n,l',r') = phase1 solver l r
    putStrLn $ "Case #" ++ show testNumber ++ ": " ++
               unwords (map show [n,l',r'])

-- | Find out how many customers are to be served from a pile,
-- and how many pancakes it will take.
--
-- The first customer id can be as low as 1 (base case for phase 1),
-- and as high as \(1414213562 = \left\lfloor\sqrt{2 \times 10^{18}}\right\rfloor\) (phase 2 after \(L=10^{18} \space R=0\)).
--
-- The interval is 1 or 2.  Same as the phase number, come to think of it.
--
-- The pile size is 0 to e18 as per input.
type Solver = Int       -- ^ first customer id
           -> Int       -- ^ interval between customers
           -> Int       -- ^ pile size
           -> (Int,Int) -- ^ (served,consumed)

-- | O(lg N) Solve by binary search on number of customers served,
-- intermediate values promoted to 'Integer'.
--
-- That's the solver I quickly wrote down after dinner after the contest.
bsInteger :: Solver
bsInteger i0 step stock = (fromIntegral n,fromIntegral (f n)) where
  n = bsearch 0 (10^!18+1) :: Integer
  f x = fromIntegral x
        * (2*fromIntegral i0 + fromIntegral step*(fromIntegral x-1))
        `div` 2 :: Integer
  bsearch a b | b-a <= 1                  = a      
              | f m <= fromIntegral stock = bsearch m b     
              | otherwise                 = bsearch a m
    where m = (a + b) `div` 2

-- | O(lg N) Solve by binary search on number of customers served.  With
-- level-headed thinking on the upper bound, this is done on 'Int'
-- values only.
bsInt :: Solver
bsInt i0 step stock = (n,f n) where
  n = bsearch 0 1414213562 -- that's ⌊ 10⁹×√2 ⌋
  f x = x * (2*i0 + step*(x-1)) `div` 2
  bsearch a b | b-a <= 1      = a      
              | f m <=  stock = bsearch m b     
              | otherwise     = bsearch a m
    where m = (a + b) `div` 2

-- | O(2) Solve using the closed-form formula.  This is so close to my
-- contest's failed attempt it still hurts.  Empirically, it either
-- falls right on or one above, so only check those two.
--
-- This is extremely dissatisfying, as it makes linear searching
-- around the sqrt result redundant.  Yet I haven't been able to
-- formulate a proper proof for it.
--
-- My intuition is currently at: if the radicand is close to a very
-- big perfect square, its square root will be that of the closest
-- double, who may very well be on the other side of the 'floor'
-- operation.  Take for example \(x = (10^9)^2 - 1\).  On reals, its
-- square root is \(10^9 - \epsilon\), so its square root's floor is
-- obviously \(10^9 - 1\).  On 'Double's, its representation is
-- \((10^9)^2\)'s, so its square root's floor is too big.
--
-- I have never observed it the other way, be it in QuickCheck or CodeJam.  I suspect this is because 
--
-- Please send help and/or a more rigorous explanation this way!
math :: Solver
math i0 step stock | f n > stock = (n,f(n-1))
                   | otherwise = (n,f n)
  where
    n = floor (sqrt (fromIntegral (2 `div` step * stock) + d^!2) - d)
    d = fromIntegral i0 / fromIntegral step - 1/2 :: Double
    f x = x * (2*i0 + step*(x-1)) `div` 2

-- | Phase 1: all customers are served from the same largest pile.
phase1 :: Solver -> Int {- ^ left -} -> Int {- ^ right -}
       -> (Int,Int,Int) {- ^ (served,left,right) -}
phase1 s l r
  | l >= r    = let (n,q) = s 1 1 (l-r) in phase2 s n (l-q) r
  | otherwise = let (n,q) = s 1 1 (r-l) in phase2 s n l (r-q)

-- | Phase 2: customers alternate from both piles.
phase2 :: Solver -> Int {- ^ served -} -> Int {- ^ left -}  -> Int {- ^ right -}
       -> (Int, Int, Int) {- ^ (served,left,right) -}
phase2 s n l r = (n + nl + nr,l - ql,r - qr) where
  ((nl,ql),(nr,qr)) | l >= r    = (s (n+1) 2 l,s (n+2) 2 r)
                    | otherwise = (s (n+2) 2 l,s (n+1) 2 r)

-- This property checking is not enough to detect the floating-point
-- precision issue! :-(
prop_bs :: Solver -> Solver -> Property
prop_bs s1 s2 = forAll gen (liftA2 (===) (uncurry3 s1) (uncurry3 s2)) where
  -- ensuring this upper bound is valid was the hard part:
  gen = liftA3 (,,) (choose (1,2 * 10 ^! 9))
                    (elements [1,2])
                    (choose (1,10 ^! 18))

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

import Data.Array.IArray
import Control.Monad
import Data.Monoid
import Data.Char

main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    m <- liftM (listArray ((1,1),(n,n)) . concat) (replicateM n getLine)
         :: IO (Array (Int,Int) Char)
    let s = accumArray mappend (Sum 0,Sum 0) (1,n) $
            map (\((t,o),c) -> (t,
                                ( if isDigit c then Sum 1 else Sum 0
                                , if c == '1' then Sum 1 else Sum 0 ))) $
            assocs m :: Array Int (Sum Int,Sum Int)
    let wp = amap (\(g,w)->fromIntegral (getSum w) / fromIntegral (getSum g)) s
        owp = listArray (1,n) [ owpc m s t | t <- [1..n] ] :: Array Int Double
        rpi = listArray (1,n) [ 0.25 * wp!i | i <- [1..n] ] :: Array Int Double
    putStrLn $ "Case #" ++ show i ++ ": " ++ show rpi

owpc m s t = sum [ wpc m s t o | o <- indices s, isDigit (m!(t,o)) ] / no
    where no = getSum $ fst (s!t)

wpc m s t o = (getSum (snd (s!o)) - if m!(o,t)=='1' then 1 else 0) /
              (getSum (fst (s!o)) - 1)
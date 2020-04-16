import Data.Bits
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  [t,w] <- map read . words <$> getLine
  replicateM_ t $ do
    -- On day D, there are Ri*2^floor(D/i) rings of type i.  So the
    -- higher the D, the more “spread out” the different Ri are in the
    -- binary representation of the number of rings for that day.  Ri
    -- is between 0 and 100, which fits in 7 bits, so we want them
    -- spread by that much.
    --
    -- The minimum D for which R6 doesn't collide with R5 is 185, so
    -- we could extract the complete R6, R5, R4 and two bits from R3
    -- from that day.  For some reason I can't remember I picked 210
    -- instead, where R6 R5 and R4 are visible as multiples of 2^53,
    -- 2^42 and 2^52 respectively, and no other Ri are visible
    -- anymore.
    --
    -- The same reasoning recovers R3, R2, R1, for example using day
    -- 38 and offsets 12, 19 and 38 respectively.  But there is a
    -- catch: all of R4, R5 and R6 will collide with R3 at that
    -- spread.  Luckily, we already know those values, so we can
    -- compensate.  (Same, for some reason I'm using 42.  Probably
    -- because it gives more round floors, but who cares in the end)
    [t210,t42] <- forM [210,42] $ \i -> do
      print i
      readLn :: IO Int
    let r1 = t42' `shiftR` 42 .&. 127
        r2 = t42' `shiftR` 21 .&. 127
        r3 = t42' `shiftR` 14 .&. 127
        t42' = t42 - r4*2^10 - r5*2^8 - r6*2^7
        r4 = t210 `shiftR` 52 .&. 127
        r5 = t210 `shiftR` 42 .&. 127
        r6 = t210 `shiftR` 35 .&. 127
    putStrLn $ unwords $ map show [r1,r2,r3,r4,r5,r6]
    "1" <- getLine
    return ()

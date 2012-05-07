import Control.Monad
import Data.Array
import qualified Data.IntMap.Strict as M
import Data.Bits
import Data.Word
import Data.List
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    putStrLn $ "Case #" ++ show i ++ ":"
    ns <- fmap (listArray (0,499) . map read . tail . words) getLine
    let (as,bs) = solve ns M.empty (combinations [0..499])
    putStrLn (unwords (map show as))
    putStrLn (unwords (map show bs))
solve ns m (c:cs) = case M.lookup s m of
                      Nothing -> solve ns (M.insert s c m) cs
                      Just c' -> if c' /= c
                                 then (expand ns c,expand ns c')
                                 else solve ns m cs
    where s = sum (expand ns c)
expand ns = map (ns!) . decode
combinations ns = map encode $ concatMap (combsOfLength ns) [1..length ns]
combsOfLength _ 0 = [[]]
combsOfLength (n:ns) k = map (n:) (combsOfLength ns (k-1)) ++ combsOfLength ns k
combsOfLength _ _ = []
data Coded = Coded {
    code0 :: !Word64 , code1 :: !Word64 , code2 :: !Word64 , code3 :: !Word64
  , code4 :: !Word64 , code5 :: !Word64 , code6 :: !Word64 , code7 :: !Word64
} deriving Eq
encode cs = Coded c0 c1 c2 c3 c4 c5 c6 c7
    where (b0,r0) = break (>= 64) cs
          (b1,r1) = break (>= 128) r0
          (b2,r2) = break (>= 192) r1
          (b3,r3) = break (>= 256) r2
          (b4,r4) = break (>= 320) r3
          (b5,r5) = break (>= 384) r4
          (b6,b7) = break (>= 448) r5
          c0 = sum $ map (2^) b0
          c1 = sum $ map ((2^) . subtract 64) b1
          c2 = sum $ map ((2^) . subtract 128) b2
          c3 = sum $ map ((2^) . subtract 192) b3
          c4 = sum $ map ((2^) . subtract 256) b4
          c5 = sum $ map ((2^) . subtract 320) b5
          c6 = sum $ map ((2^) . subtract 384) b6
          c7 = sum $ map ((2^) . subtract 448) b7
decode c = b0 ++ map (+64) b1 ++ map (+128) b2 ++ map (+192) b3 ++
           map (+256) b4 ++ map (+320) b5 ++ map (+384) b6 ++ map (+448) b7
    where [b0,b1,b2,b3,b4,b5,b6,b7] = map (f . ($c))
                                      [code0,code1,code2,code3
                                      ,code4,code5,code6,code7]
          f p = filter (testBit p) [0..63]

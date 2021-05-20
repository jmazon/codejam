import           Control.Monad
import           Data.Tree
import           Data.Vector (Vector,(!))
import qualified Data.Vector as V
import qualified Data.IntSet as IntSet

m :: Int
m = 1000000007

backtrack :: Int -> [Int] -> Int
backtrack n = length . foldM add (0,[],IntSet.empty)
  where
    add (v,vss,cl) v' = case compare v' (v+1) of
      GT -> mzero
      EQ -> do
        s <- lower vss
        guard (s `IntSet.notMember` cl)
        pure (v',s : vss,IntSet.insert s cl)
      LT -> do
        let vss' = drop (v - v') vss
        s <- higher vss'
        guard (s `IntSet.notMember` cl)
        pure (v',s : tail vss',IntSet.insert s cl)
    lower [] = [1..n]
    lower (s:_) = [1..s-1]
    higher [s] = [s+1..n]
    higher (a:b:_) = [a+1..b-1]

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    n <- readLn
    vs <- map read . words <$> getLine
    let answer | n <= 13 = backtrack n vs
               | otherwise = solve vs
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

solve :: [Int] -> Int
solve = maybe 0 (fst . foldTree count . cover . snd) . foldM process (0,[])

type Stack = (Int,[Tree ()])

process :: Stack -> Int -> Maybe Stack
process (v,s) v' = case compare v' (v+1) of
  GT -> Nothing
  EQ -> Just (v',Node () [] : s)
  LT -> let (covered,s') = splitAt (v - v' + 1) s
       in Just (v',Node () [cover covered] : s')

cover :: [Tree ()] -> Tree ()
cover (t : Node r f : ts) = cover (Node r (t:f):ts)
cover [t] = t
cover [] = error "cover: empty list"

count :: () -> [(Int,Int)] -> (Int,Int)
count () [] = (1,1)
count () [(c,n)] = (c,n+1)
count () [(c1,n1),(c2,n2)] = (combinations (n1+n2) n1 *% c1 *% c2,1+n1+n2)
count _ _ = error "count: tree isn't binary"

(*%) :: Int -> Int -> Int
(*%) = ((`mod` m) .) . (*)
infixl 8 *%

combinations :: Int -> Int -> Int
combinations n k = fact!n *% invFact!(n-k) *% invFact!k

nFact :: Int
nFact = 100000

fact :: Vector Int
fact = V.scanl (*%) 1 $ V.generate nFact succ

invFact :: Vector Int
invFact = V.scanr (*%) 716327852 $ V.generate nFact succ

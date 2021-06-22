import Control.Monad (forM_,replicateM)
import Data.List     (find)

readPair :: String -> (Int,Int)
readPair l = (a,b) where [a,b] = map read (words l)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    m <- readLn
    ps <- replicateM m (readPair <$> getLine)
    putStrLn $ "Case #" ++ show i ++ ": " ++ maybe "0" show (solveLarge ps)

type Factors = [(Int,Int)]
type Solver = Factors -> Maybe Int

fSum :: Factors -> Int
fSum = sum . map (uncurry (*))

solveLarge :: Solver
solveLarge fs = find isSol [total-2,total-3..total-bound*fst (last fs)]
  where total = fSum fs
        bound = ceiling (logBase (fromIntegral (fst (head fs))) (fromIntegral total))
        isSol s | Just ps@(_:_) <- factor fs s = total == s + fSum ps
        isSol _ = False

factor :: Factors -> Int -> Maybe Factors
factor = go where
  go _ i | i < 1 = Nothing
  go _ 1 = Just []
  go [] _ = Nothing
  go ((p,l):ps) i = case takeWhile ((== 0) . snd) $ iterate ((`divMod` p) . fst) (i,0) of
    [_] -> go ps i
    fs -> let c = length fs - 1
         in if c <= l then fmap ((p,length fs - 1) :) (go ps (fst (last fs))) else Nothing

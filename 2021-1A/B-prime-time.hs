import Control.Monad (forM_,replicateM)
import Data.List     (find)

type Factor = (Int,Int)
readPair :: String -> Factor
readPair l = (a,b) where [a,b] = map read (words l)

factor :: [Factor] -> Int -> Maybe [Factor]
factor = go where
  go _ i | i < 1 = Nothing
  go _ 1         = Just []
  go [] _        = Nothing
  go ((p,c):ps) i =
    let qrs = takeWhile ((== 0) . snd) $ tail $ iterate ((`divMod` p) . fst) (i,0)
    in case qrs of
         [] -> go ps i
         fs | c' <- length fs ->
                if c' <= c
                then ((p,c') :) <$> go ps (fst (last fs))
                else Nothing

solve :: [Factor] -> Maybe Int
solve fs = find isSol [total-2,total-3..total-bound*fst (last fs)]
  where total = fSum fs
        bound = ceiling $ logBase (fromIntegral (fst (head fs)))
                                  (fromIntegral total :: Double)
        isSol s | Just ps@(_:_) <- factor fs s = total - fSum ps == s
        isSol _ = False
        fSum = sum . map (uncurry (*))

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    m <- readLn
    ps <- replicateM m (readPair <$> getLine)
    putStrLn $ "Case #" ++ show i ++ ": " ++ maybe "0" show (solve ps)

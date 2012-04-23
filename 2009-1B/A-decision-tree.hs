import Control.Monad
newtype Tree = T (Double,Maybe (String,Tree,Tree))
main = do
  n <- readLn
  forM_ [1..n] $ \i -> do
    putStrLn $ "Case #" ++ show i ++ ":"
    l <- readLn
    t <- fmap (fst . tree . tokenize . concat) (replicateM l getLine)
    a <- readLn
    replicateM_ a $ do
      fs <- fmap (drop 2 . words) getLine
      print (eval fs t)
eval fs (T (w,Nothing)) = w
eval fs (T (w,Just (f,t1,t2))) = w * eval fs (if f `elem` fs then t1 else t2)
tree ("(" : w : ")" : ts) = (T (read w,Nothing),ts)
tree ("(" : w : f : ts) = (T (read w,Just (f,t1,t2)),tail ts'')
    where (t1,ts') = tree ts
          (t2,ts'') = tree ts'
tokenize = words . t
    where t (c:cs) | c `elem` "()" = ' ':c:' ' : t cs
                   | otherwise     = c : t cs
          t _ = ""
-- SUCCESS

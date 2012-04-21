import Control.Monad
import Text.ParserCombinators.ReadP
data DTree = DTree Double (Maybe (String, DTree, DTree))
decision :: ReadP DTree
decision = readS_to_P $ readParen True $ readP_to_S $ do
  weight <- readS_to_P reads
  c <- (do feature <- readS_to_P lex
           true <- decision
           false <- decision
           return $ Just (feature,true,false)) <++ return Nothing
  return $ DTree weight c
eval :: DTree -> [String] -> Double
eval (DTree w c) fs = w*maybe 1(\(f,y,n)->eval(if elem f fs then y else n)fs)c
main=do
  n <- readLn
  forM_ [1..n] $ \t -> do
    putStrLn $ "Case #" ++ show t ++ ":"
    l <- readLn
    ls <- fmap unlines $ replicateM l getLine
    let [(tree,_)] = readP_to_S decision ls
    n <- readLn
    replicateM n (print.eval tree.drop 2.words=<<getLine)
  
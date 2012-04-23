import Control.Monad
import Text.ParserCombinators.Parsec
data F=N|F String(Float,F)(Float,F)
feature=do
  i <- many1 letter
  whiteSpace
  t <- decision
  t' <- decision
  return (F i t t')
decision=do
  char '('
  whiteSpace
  g <- many1 (oneOf ".0123456789")
  whiteSpace
  p <- (feature<|>return N)
  char ')'
  whiteSpace
  return (read g,p)
whiteSpace=many (oneOf " \n")
main=do
  n <- readLn
  forM_ [1..n] $ \t -> do
    putStrLn $ "Case #" ++ show t ++ ":"
    l <- readLn
    ls <- replicateM l getLine
    let (Right tree) = parse decision "" $ join ls
    fs <- readLn
    replicateM fs (process tree)
process tree = do
  fs <- fmap (drop 2 . words) getLine
  print $ eval tree fs
eval (weight,tree) =(weight*).eval' tree
eval' N _=1
eval' (F f y n) x = eval (if f `elem` x then y else n) x


  
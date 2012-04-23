import Control.Monad
import Control.Applicative
import Data.List
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    r <- fmap (solve . parse . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": [" ++ intercalate ", " (reverse $ map (:"") r) ++ "]"
parse (nc:l) = ( map (\(a:b:c:_) -> ((a,b),c)) cs
          , map (\(a:b:_) -> (a,b)) ds
          , w )
    where (cs,nd:l') = splitAt (read nc) l
          (ds,[_,w]) = splitAt (read nd) l'
solve (cs,ds,w) = foldl f [] w
    where f (e:es) e' = case compose e e' of
                          Just e'' -> e'':es
                          Nothing -> if any (oppose e') (e:es)
                                     then []
                                     else (e':e:es)
          f _ e = [e]
          compose a b = compose' a b <|> compose' b a
          compose' a b = fmap snd $ find ((==(a,b)) . fst) cs
          oppose a b = elem (a,b) ds || elem (b,a) ds

test1 = solve $ parse $ words "0 0 2 EA"
test2 = solve $ parse $ words "1 QRI 0 4 RRQR"
test3 = solve $ parse $ words "1 QFT 1 QF 7 FAQFDFQ"
test4 = solve $ parse $ words "1 EEZ 1 QE 7 QEEEERA"
test5 = solve $ parse $ words "0 1 QW 2 QW"




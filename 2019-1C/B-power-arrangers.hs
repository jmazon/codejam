import Data.List
import Control.Monad
import System.IO
import System.Exit

data Figure = A|B|C|D|E deriving (Show,Read,Eq,Ord)

getGCJ = do r <- getLine
            when (r == "N") exitSuccess
            return r

getAt j i = do print (5*(i-1) + j)
               read <$> getGCJ

main = do
  hSetBuffering stdout LineBuffering
  [t,f] <- map read . words <$> getGCJ
  replicateM_ t $ do
    as <- mapM (getAt 1) [1..119]
    let Just (a:_) = find ((== 23) . length) $ group $ sort as
        ais = elemIndices a as
    bs <- mapM (getAt 2 . succ) ais
    let Just (b:_) = find ((== 5) . length) $ group $ sort bs
        bis = elemIndices b bs
    cs <- mapM (getAt 3 . succ . (ais !!)) bis
    let Just [c] = find ((== 1) . length) $ group $ sort cs
        cis = elemIndices c cs
    [e] <- mapM (getAt 4 . succ . (ais !!) . (bis !!)) cis
    let [d] = [A,B,C,D,E] \\ [a,b,c,e]
    putStrLn $ show =<< [a,b,c,d,e]
    getGCJ

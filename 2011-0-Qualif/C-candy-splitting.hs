import Control.Monad
import Data.Bits
main = mapM_ putStrLn . zipWith format [1..] . map solve . parse . tail . map read . words =<< getContents
format i x = "Case #" ++ show i ++ ": " ++ maybe "NO" show x
parse (n:ns) = let (p,ns') = splitAt n ns in p : parse ns'
parse _ = []
solve ns = guard (foldl1 xor ns == 0) >> return (sum ns - minimum ns)
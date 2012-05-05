import Control.Monad
import Data.List
main = zipWithM_ format [1..] . map solve .
       unfoldr parse . map read . tail . words =<< getContents
format i j = putStrLn $ "Case #" ++ show i ++ ": " ++ unwords (map show j)
parse (n:ns) = Just (map fromIntegral js,ns') where (js,ns') = splitAt n ns
parse _ = Nothing
solve js = map (bsearch 0 100) js
    where x = sum js
          bsearch a b j | b - a < 10**(-7) = 100 * b
                        | otherwise       = if isEnough c j
                                            then bsearch a c j
                                            else bsearch c b j
              where c = (a + b) / 2
          isEnough y j = isEnough' s (1-y) (filter (<= s) (delete j js))
              where s = j + x*y
          isEnough' s y js | y < 0     = True
                           | null js   = False
                           | otherwise = isEnough' s (y - (s - head js) / x) 
                                                   (tail js)

import qualified Data.Set as S
main = interact $ unlines . zipWith format [1..] . map solve . parse
parse = tail . map read . words
format i j = "Case #" ++ show i ++ ": " ++ maybe "INSOMNIA" show j
solve 0 = Nothing
solve n = go S.empty n where
  go s m | S.size s' == 10 = Just m
         | otherwise       = go s' (n+m) where
    s' = foldl (flip S.insert) s (show m)

main = mapM_ putStrLn . zipWith format [1..] . map solve . parse . map read . tail . words =<< getContents
format i n = "Case #" ++ show i ++ ": " ++ show n ++ ".00000"
parse (n:ns) = let (p,ns') = splitAt n ns in p : parse ns'
parse _ = []
solve = length . filter (uncurry (/=)) . zip [1..]
main = interact (unlines . zipWith c [1..] . map (unwords . reverse . words) . tail . lines)
c i j = "Case #" ++ show i ++ ": " ++ j

-- SUCCESS

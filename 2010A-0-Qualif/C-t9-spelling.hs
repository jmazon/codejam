main = interact (unlines . zipWith c [1..] . map t9 . tail . lines)
c i j = "Case #" ++ show i ++ ": " ++ j

t9 = merge . map e
e 'a' = "2"
e 'b' = "22"
e 'c' = "222"
e 'd' = "3"
e 'e' = "33"
e 'f' = "333"
e 'g' = "4"
e 'h' = "44"
e 'i' = "444"
e 'j' = "5"
e 'k' = "55"
e 'l' = "555"
e 'm' = "6"
e 'n' = "66"
e 'o' = "666"
e 'p' = "7"
e 'q' = "77"
e 'r' = "777"
e 's' = "7777"
e 't' = "8"
e 'u' = "88"
e 'v' = "888"
e 'w' = "9"
e 'x' = "99"
e 'y' = "999"
e 'z' = "9999"
e ' ' = "0"

merge (a:b:xs) | last a == head b = a ++ " " ++ merge (b:xs)
               | otherwise        = a ++ merge (b:xs)
merge [xs] = xs

-- SUCCESS

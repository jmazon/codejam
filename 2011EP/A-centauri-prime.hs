import Data.Char
main = interact $ unlines . zipWith format [1..] . map solve . tail . words
format i s = "Case #" ++ show i ++ ": " ++ s
solve l | l' `elem` "aeiou" = l ++ " is ruled by a queen."
        | l' == 'y'         = l ++ " is ruled by nobody."
        | otherwise         = l ++ " is ruled by a king."
    where l' = toLower (last l)
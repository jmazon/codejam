import Data.List (foldl1',elemIndex)
data B = O | I | J | K deriving Eq
data Q = Q Bool B deriving Eq
mul (Q s1 O) (Q s2 x) = Q (s1 /= s2) x
mul (Q s1 x) (Q s2 O) = Q (s1 /= s2) x
mul (Q s1 x) (Q s2 y) | x == y = Q (s1 == s2) O
mul (Q s1 I) (Q s2 J) = Q (s1 /= s2) K
mul (Q s1 J) (Q s2 K) = Q (s1 /= s2) I
mul (Q s1 K) (Q s2 I) = Q (s1 /= s2) J
mul q1 q2 = Q (not s) z where Q s z = mul q2 q1
one = Q False O ; i = Q False I ; j = Q False J ; k = Q False K
                ; rd 'i' = i    ; rd 'j' = j    ; rd 'k' = k
main = interact $ unlines . zipWith format [1..] . map (uncurry3 solve) .
                  parse . tail . words
format i j = "Case #" ++ show i ++ ": " ++ if j then "YES" else "NO"
parse (l : x : s : ws) = (read l,read x,map rd s) : parse ws
parse _ = []
solve l x s = pp == Q True O && is + ks < l*x where
  p = foldl1' mul s
  pp = iterate (`mul` p) one !! (x `mod` 4)
  is = maybe (l*x) id $ elemIndex i $ scanl mul one $ concat $ replicate 4 s
  ks = maybe (l*x) id $ elemIndex k $ scanl (flip mul) one $ concat $ replicate 4 $ reverse s
uncurry3 f (x,y,z) = f x y z

winning a b = case a `compare` b of
                EQ -> False
                LT -> winning b a
                GT -> a `mod` b > gcd a b

module CollatzConjecture (collatz) where

count :: Integer -> Integer -> Maybe Integer
count c 1 = Just c
count c n 
    | n == 0 = Nothing
    | n < 0 = Nothing
    | even n = count (c + 1) (n `div` 2)
    | odd n  = count (c + 1) (3 * n + 1)
    | otherwise = Nothing

collatz :: Integer -> Maybe Integer
collatz = count 0 

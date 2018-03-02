module SumOfMultiples (sumOfMultiples) where

isMultiples :: Integer -> Integer -> Bool
isMultiples x y = x `mod` y == 0

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples _  1 = 0
sumOfMultiples factors limit = sum $ takeWhile (<limit) [x | x <- [1..], any (isMultiples x) factors]

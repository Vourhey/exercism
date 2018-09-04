module PrimeFactors (primeFactors) where

recursiveChecks :: Integer -> Integer -> [Integer]
recursiveChecks 1 _ = []
recursiveChecks n d = 
    if n `mod` d == 0 then 
        d : recursiveChecks (n `div` d) d 
    else 
        recursiveChecks n (d + 1)

primeFactors :: Integer -> [Integer]
primeFactors n = recursiveChecks n 2

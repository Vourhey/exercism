module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

doDigits :: [Int] -> [Int]
doDigits digits = fst $ 
    foldr (\y (acc, b) -> (if b then double (y*2) : acc else y : acc, not b)) 
          ([], False) 
          digits
    where
        double n = if n > 9 then n - 9 else n 

isValid :: String -> Bool
isValid n
    | length s < 2 = False
    | (length $ filter (not . isDigit) s) > 0 = False
    | otherwise = (sum $ doDigits digits) `mod` 10 == 0
    where 
        s = concat $ words n
        digits = map digitToInt s


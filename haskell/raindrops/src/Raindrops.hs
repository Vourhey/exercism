module Raindrops (convert) where

convert :: Int -> String
convert n 
    | null result = show n
    | otherwise = result
    where
        result = factor3 ++ factor5 ++ factor7
        factor3 = if n `mod` 3 == 0 then "Pling" else ""
        factor5 = if n `mod` 5 == 0 then "Plang" else ""
        factor7 = if n `mod` 7 == 0 then "Plong" else ""

module Diamond (diamond) where

import Data.Char

row :: Char -> Char -> String
row 'A' 'A' = "A"
row l 'A' = spaces ++ ['A'] ++ spaces
    where
        spaces = replicate (ord l - ord 'A') ' ' 
row l c = replicate timesSpace ' ' ++ c:[] ++ replicate spaceBetween ' ' ++ c:[] ++ replicate timesSpace ' '
    where 
        timesSpace = ord l - ord c
        spaceBetween = 2 * (ord c - ord 'A') - 1

halfDiamond :: Char -> [String]
halfDiamond l = map (row l) ['A'..l]

diamond :: Char -> Maybe [String]
diamond l 
    | isAlpha l = Just $ (half ++ (tail $ reverse half))
    | otherwise = Nothing
    where half = halfDiamond l

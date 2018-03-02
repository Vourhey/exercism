module Acronym (abbreviate) where

import Data.List.Split
import Data.Char

getLetters :: String -> String
getLetters w = if check then [first] else first : filtered
    where
        first = toUpper $ head w
        filtered = filter isUpper (tail w)
        check = length (tail w) == length filtered

abbreviate :: String -> String
abbreviate xs = 
    let ws = filter (not . null) $ splitOneOf " ,-" xs
    in concat $ map getLetters ws

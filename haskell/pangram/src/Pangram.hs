module Pangram (isPangram) where

import Data.Char
import Data.List

alphabet = ['a'..'z']

isPangram :: String -> Bool
isPangram text 
    | null text = False
    | otherwise = check
    where 
        s = map toLower (filter isAlpha text)
        s' = sort $ nub s
        check = alphabet == s'

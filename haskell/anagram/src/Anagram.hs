module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor _  []      = [] 
anagramsFor xs (x:xss) = 
    if sortedxs == sortedx && lowerxs /= lowerx
        then x : anagramsFor xs xss 
        else anagramsFor xs xss 
    where 
        lowerxs = map toLower xs
        lowerx = map toLower x 
        sortedxs = sort lowerxs
        sortedx = sort lowerx

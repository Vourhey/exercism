module Isogram (isIsogram) where

import Data.List (nub)
import Data.Char (toLower, isAlpha)

isIsogram :: String -> Bool
isIsogram str = (length $ nub s) == (length s)
    where s = map toLower $ filter isAlpha str

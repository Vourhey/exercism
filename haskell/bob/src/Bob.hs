module Bob (responseFor) where

import Data.List
import Data.Char

isStringUpper :: String -> Bool
isStringUpper "" = False
isStringUpper xs = all isUpper xs

responseFor :: String -> String
responseFor s
  | null $ filter (not . isSpace) s = "Fine. Be that way!"
  | upperString s && lc == '?' = "Calm down, I know what I'm doing!"
  | lc == '?' = "Sure."
  | upperString s = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    lc = last $ reverse $ dropWhile isSpace (reverse s)
    upperString l = isStringUpper $ filter isAlpha l

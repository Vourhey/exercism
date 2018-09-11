module Atbash (decode, encode) where

import Data.Char (isAlphaNum, isAlpha, ord, chr, toLower)
import Data.List.Split (chunksOf)

lettera = ord 'a'
boundary = lettera + 25

doCipher :: String -> String
doCipher []     = []
doCipher (x:xs) = if isAlpha x then
                    chr (boundary - ord x + lettera) : doCipher xs
                  else
                    x : doCipher xs

prepareText :: String -> String
prepareText text = map toLower $ filter isAlphaNum text

decode :: String -> String
decode cipherText = doCipher $ prepareText cipherText

encode :: String -> String
encode plainText = unwords $ 
                    chunksOf 5 $ 
                        doCipher (prepareText plainText)

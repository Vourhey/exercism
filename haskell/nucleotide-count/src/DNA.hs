module DNA (nucleotideCounts) where

import qualified Data.Map as Map  

count :: Eq a => a -> [a] -> Int 
count x = length . filter (==x) 

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts xs = if (length $ filter (`elem` "ACGT") xs) /= length xs
                        then Left "Error"
                        else Right $ Map.fromList [('A', count 'A' xs),
                                                   ('C', count 'C' xs),
                                                   ('G', count 'G' xs),
                                                   ('T', count 'T' xs)]

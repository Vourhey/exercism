module ETL (transform) where

import Data.Map (Map, foldrWithKey, fromList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList $ foldrWithKey f [] legacyData
    where f k a result = result ++ (map (\x -> (toLower x, k)) a)

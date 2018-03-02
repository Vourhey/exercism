module DNA (toRNA) where

change :: Char -> Maybe Char
change 'G' = Just 'C'
change 'C' = Just 'G'
change 'T' = Just 'A'
change 'A' = Just 'U'
change _ = Nothing

toRNA :: String -> Maybe String
toRNA xs =  sequence $ fmap change xs

module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
    | year400 = True
    | yearNot100 && year4 = True
    | otherwise = False
    where 
        year400    = year `mod` 400 == 0
        yearNot100 = year `mod` 100 /= 0
        year4      = year `mod` 4 == 0


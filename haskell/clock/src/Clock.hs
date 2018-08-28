module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock Int deriving (Eq, Show)

instance Num Clock where
    fromInteger x = fromHourMin 0 (fromInteger x)
    (+) (Clock m1) (Clock m2) = fromHourMin 0 $ m1 + m2
    negate (Clock m) = Clock $ 1440 - m

clockHour :: Clock -> Int
clockHour (Clock 1440) = 0
clockHour (Clock m) = m `div` 60

clockMin :: Clock -> Int
clockMin (Clock m) = m `mod` 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ (hour * 60 + min) `mod` 1440


toString :: Clock -> String
toString clock = hours ++ ":" ++ minutes 
    where hour = clockHour clock
          min = clockMin clock
          hours = if hour < 10 then "0" ++ show hour else show hour
          minutes = if min < 10 then "0" ++ show min else show min
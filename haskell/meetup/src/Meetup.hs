module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
  let monthlength = gregorianMonthLength year month
      weekdays    = map (toWeekDate . fromGregorian year month) [1 .. monthlength]
      filtered    = filter (\(_,_,z) -> z == weekdayToInt weekday) weekdays
      days        = map (\(y,w,d) -> fromWeekDate y w d) filtered
  in
    case schedule of 
      First  -> head days
      Second -> days !! 1
      Third  -> days !! 2
      Fourth -> days !! 3
      Last   -> last days
      Teenth -> head $ filter ((\(y, m, d) -> d `elem` [13..19]) . toGregorian) days

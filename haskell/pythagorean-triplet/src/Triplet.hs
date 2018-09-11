module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort, group)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = if (da + db) == dc then True else False
    where
        da = a * a
        db = b * b
        dc = c * c

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (sorted !! 0, sorted !! 1, sorted !! 2)
    where 
        sorted = sort [a, b, c]

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor = unique $ filter isPythagorean [mkTriplet x y z | x <- boundaries, y <- boundaries, z <- boundaries, x*x + y*y == z*z]
    where
        unique = map head . group . sort
        boundaries = [minFactor .. maxFactor]

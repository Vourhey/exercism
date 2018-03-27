module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

checkIfIllegal :: Real a => a -> a -> a -> Bool
checkIfIllegal a b c = 
    if a + b > c &&
       a + c > b &&
       b + c > a 
       then False
       else True    -- True means it's illegal

typeOfTriangle :: Real a => [a] -> TriangleType
typeOfTriangle sides = 
    if (sides !! 0) == (sides !! 1) && (sides !! 1) == (sides !! 2)
        then Equilateral
        else if sides !! 0 == sides !! 1 || sides !! 1 == sides !! 2 
            then Isosceles
            else Scalene

triangleType :: Real a => a -> a -> a -> TriangleType
triangleType a b c = 
    let sides = sort [a,b,c]
    in
        if checkIfIllegal a b c 
            then Illegal
            else typeOfTriangle sides


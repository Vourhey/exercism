module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits as B

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Enum)

allergiesRecursive :: Int -> Int -> [Allergen]
allergiesRecursive score (-1) = []
allergiesRecursive score current = 
    if B.testBit score current then 
        toEnum current : rest
    else
        rest
    where rest = allergiesRecursive score (current - 1)

allergies :: Int -> [Allergen]
allergies score = allergiesRecursive score 7

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = B.testBit score b
    where b = fromEnum allergen

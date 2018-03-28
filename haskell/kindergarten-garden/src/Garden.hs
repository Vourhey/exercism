module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

defaultClass = ["Alice", "Bob", "Charlie", "David",
                "Eve", "Fred", "Ginny", "Harriet",
                "Ileana", "Joseph", "Kincaid", "Larry"]

toPlant :: Char -> Plant
toPlant = plant
    where plant 'C' = Clover
          plant 'G' = Grass
          plant 'R' = Radishes
          plant 'V' = Violets

parseRows :: String -> String -> [String] -> Map String [Plant] -> Map String [Plant]
parseRows [] [] _ result = result 
parseRows row1 row2 students result = Map.insert name plants (parseRows nRow1 nRow2 nStudents result)
    where firstHalf = map toPlant $ take 2 row1
          secondHalf = map toPlant $ take 2 row2
          plants = firstHalf ++ secondHalf 
          name = head students
          nRow1 = drop 2 row1
          nRow2 = drop 2 row2
          nStudents = drop 1 students

defaultGarden :: String -> Map String [Plant]
defaultGarden plants = parseRows row1 row2 defaultClass Map.empty
    where rows = lines plants
          row1 = head rows
          row2 = last rows

garden :: [String] -> String -> Map String [Plant]
garden students plants = parseRows row1 row2 (sort students) Map.empty
    where rows = lines plants
          row1 = head rows
          row2 = last rows

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student garden = garden Map.! student

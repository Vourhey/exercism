module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import qualified Data.Vector as V

data Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq, Show)

vec :: Matrix a -> V.Vector (V.Vector a)
vec (Matrix rows) = rows 

cols :: Matrix a -> Int
cols (Matrix v) = if V.length v > 0 then
                    V.length $ V.head v
                  else 0

column :: Int -> Matrix a -> V.Vector a
column x (Matrix v) = V.map (V.! x) v

flatten :: Matrix a -> V.Vector a
flatten (Matrix rows) = V.fromList $ V.foldl (\x y -> x ++ (V.toList y)) [] rows 

fromList :: [[a]] -> Matrix a
fromList []  = Matrix (V.empty)
fromList (x:xss) = Matrix $ V.cons (V.fromList x) (vec $ fromList xss) 

fromString :: Read a => String -> Matrix a
fromString xs = fromList arrayOfArrays
    where 
        rows = lines xs
        stringNumbers = map words rows
        arrayOfArrays = map (map read) stringNumbers


reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) matrix = fromList $ split c flat
    where 
        flat = V.toList $ flatten matrix
        split n f
            | null f = []
            | otherwise = (take n f) : (split n (drop n f)) 

row :: Int -> Matrix a -> V.Vector a
row x (Matrix rows) = rows V.! x

rows :: Matrix a -> Int
rows (Matrix v) = V.length v

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix $ go (cols matrix - 1) matrix
    where 
        go c m
            | c < 0     = V.fromList []
            | otherwise = V.snoc (go (c - 1) m) (column c m)

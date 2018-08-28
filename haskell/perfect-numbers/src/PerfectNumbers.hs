module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
    | n < 1 = Nothing
    | s == n = Just Perfect
    | s > n = Just Abundant
    | s < n = Just Deficient
    where
        s = sum [x | x <- [1..(n-1)],  n `mod` x == 0]

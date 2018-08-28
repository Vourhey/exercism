module SecretHandshake (handshake) where

shakes :: [String]
shakes = ["wink", "double blink", "close your eyes", "jump"]

toShakes :: Int -> [String] -> [ String ]
toShakes _ [] = []
toShakes 0 _ = []
toShakes n dict 
    | n `rem` 2 == 1 = head dict : next
    | otherwise = next
    where next = toShakes (n `quot` 2 ) (drop 1 dict)

handshake :: Int -> [String]
handshake n 
    | n `quot` 16 == 1 = reverse result
    | otherwise = result
    where result = toShakes n shakes

module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

import Data.List (reverse)

data LinkedList a = Node a (LinkedList a) | NIL deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Node a linkedList) = a

fromList :: [a] -> LinkedList a
fromList []     = NIL
fromList (x:xs) = Node x (fromList xs)

isNil :: LinkedList a -> Bool
isNil NIL = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Node x linkedList

next :: LinkedList a -> LinkedList a
next (Node a linkedList) = linkedList

nil :: LinkedList a
nil = NIL

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = fromList $ reverse $ toList linkedList

toList :: LinkedList a -> [a]
toList NIL                 = []
toList (Node a NIL)        = [a]
toList (Node a linkedList) = a : toList linkedList

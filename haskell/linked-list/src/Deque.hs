module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Deque a = Deque (IORef [a])

mkDeque :: IO (Deque a)
mkDeque = do
    empty <- newIORef []
    return $ Deque empty

pop :: Deque a -> IO (Maybe a)
pop (Deque d) = do
    queue <- readIORef d
    if null queue then
        return Nothing
    else
        do 
            modifyIORef d init
            return $ Just $ last queue

push :: Deque a -> a -> IO ()
push (Deque d) x = do
    queue <- readIORef d
    writeIORef d (queue ++ [x])

unshift :: Deque a -> a -> IO ()
unshift (Deque d) x = do
    queue <- readIORef d
    writeIORef d (x : queue) 

shift :: Deque a -> IO (Maybe a)
shift (Deque d) = do
    queue <- readIORef d
    if null queue then
        return Nothing
    else
        do
            modifyIORef d tail
            return $ Just $ head queue

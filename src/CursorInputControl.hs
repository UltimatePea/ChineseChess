{-# LANGUAGE FlexibleInstances #-}
module CursorInputControl where


import Control.Monad.State.Lazy
import System.IO
import ApplicationDeclaration
import System.Console.ANSI

controlMainLoop :: Application
controlMainLoop = do
    liftIO $ hSetBuffering stdin NoBuffering
    isEof <- liftIO $ hIsEOF stdin
    if isEof -- also check hReady, e.g. up arrow is esc[a
    then return ()
    else do
        c <- liftIO $ hGetChar stdin
        handle c
        controlMainLoop

handle :: Char -> Application
handle 'h' = undefined


type CurorPosition = (Int, Int)


maxX = 8 -- fixed width for now
maxY = 9 -- fixed height for now

class MoveAction a where
    move :: a -> (Int, Int) -> (Int, Int)


class Monad m => MonadPosition m where
    getPosition :: m (Int, Int)
    putPosition :: (Int, Int) -> m ()

instance (Monad m) => MonadPosition (StateT (Int, Int) m) where
    getPosition = get
    putPosition = put


moveCursorRaw :: (Monad m, MoveAction t) => t -> StateT (Int, Int) m ()
moveCursorRaw action = do
    (x, y) <- get
    put (move action (x,y))

updateScreenCursorLocationRaw :: (MonadIO m) => StateT (Int, Int) m ()
updateScreenCursorLocationRaw = do
    (x, y) <- get
    liftIO $ setCursorPosition (x * 2 - 1) (y * 2 - 1)

data MoveActions = MoveLeft | MoveRight | MoveUp | MoveDown

instance MoveAction MoveActions where
    move MoveLeft (x, y) 
                | x > 0 = (x-1, y)
                | x == 0 = (x, y)
    move MoveRight (x, y) 
                | x < maxX = (x+1, y)
                | x == maxX = (x, y)
    move MoveUp (x, y) 
                | y > 0 = (x, y-1)
                | y == 0 = (x, y)
    move MoveDown (x, y) 
                | y < maxY = (x, y+1)
                | y == maxY = (x, y)

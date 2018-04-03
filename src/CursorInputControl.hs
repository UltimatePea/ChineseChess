{-# LANGUAGE FlexibleInstances #-}
module CursorInputControl where


import Control.Monad.State.Lazy
import ApplicationDeclaration
import System.Console.ANSI



type CurorPosition = (Int, Int)


maxX = 9 -- x is down
maxY = 8 -- y is across

class MoveAction a where
    move :: a -> (Int, Int) -> (Int, Int)


class Monad m => MonadPosition m where
    getPosition :: m (Int, Int)
    putPosition :: (Int, Int) -> m ()

instance (Monad m) => MonadPosition (StateT (Int, Int) m) where
    getPosition = get
    putPosition = put


moveCursor :: (MonadPosition m, MoveAction t) => t ->  m ()
moveCursor action = do
    (x, y) <- getPosition
    putPosition (move action (x,y))

updateScreenCursorLocation :: (MonadIO m, MonadPosition m) => m ()
updateScreenCursorLocation = do
    (x, y) <- getPosition
    liftIO $ setCursorPosition ((x+1) * 2 - 1) ((y+1) * 2 - 1)

printPositionInfo :: (MonadIO m, MonadPosition m) => m ()
printPositionInfo = do
    (x, y) <- getPosition
    liftIO $ putStrLn $ "Cursor At: (" ++ show x ++ ", " ++ show y ++ ")"


data MoveActions = MoveLeft | MoveRight | MoveUp | MoveDown

instance MoveAction MoveActions where
    move MoveUp (x, y) 
                | x > 0 = (x-1, y)
                | x == 0 = (x, y)
    move MoveDown (x, y) 
                | x < maxX = (x+1, y)
                | x == maxX = (x, y)
    move MoveLeft (x, y) 
                | y > 0 = (x, y-1)
                | y == 0 = (x, y)
    move MoveRight (x, y) 
                | y < maxY = (x, y+1)
                | y == maxY = (x, y)

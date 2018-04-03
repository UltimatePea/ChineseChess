{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module CursorInputControl where


import AppState
import Control.Monad.State.Lazy
import ApplicationDeclaration
import System.Console.ANSI hiding (cursorPosition)



type CurorPosition = (Int, Int)


maxX = 9 -- x is down
maxY = 8 -- y is across

class MoveAction a where
    move :: a -> (Int, Int) -> (Int, Int)


class Monad m => MonadPosition m where
    getPosition :: m (Int, Int)
    putPosition :: (Int, Int) -> m ()

instance (Monad m, MonadState RootStore m) => MonadPosition  m where
    getPosition = do
        store <- get
        return (cursorPosition store)
    putPosition p = do
        store <- get
        put (store  { cursorPosition = p })


moveCursor :: (MonadPosition m, MoveAction t) => t ->  m ()
moveCursor action = do
    (x, y) <- getPosition
    putPosition (move action (x,y))

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

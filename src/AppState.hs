{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module AppState where

import Control.Monad.State.Lazy


data PieceType = General | Advisor | Elephant | Horse | Chariot | Cannon | Soldier | Empty deriving Eq
data PieceSide = Red | Black | None deriving (Eq, Show)
data Piece = Piece PieceSide PieceType

data RawBoard a = RawBoard [[a]]
type Board = RawBoard Piece

data AppState = Normal | PieceSelected {
    selectedPosition :: (Int, Int),
    reachablePositions :: [(Int, Int)]
    } | End | AppError String | OperationSuccessful String

data GameState = NotInGame | InGame { currentSide :: PieceSide} | GameFinished {winningSide :: PieceSide}

data RootStore = RootStore
    {
          cursorPosition :: (Int, Int)
        , board :: Board
        , appState :: AppState
        , movementAction :: ((Int, Int), (Int, Int))
        , gameState :: GameState
    }

class Monad m => MonadRootStore m where
    getRootStore :: m RootStore
    putRootStore :: RootStore -> m ()

instance (Monad m, MonadState RootStore m) => MonadRootStore  m where
    getRootStore = get
    putRootStore = put

class Monad m => MonadAppState m where
    getAppState :: m AppState
    putAppState :: AppState -> m ()

instance (Monad m, MonadRootStore m) => MonadAppState  m where
    getAppState = do
        store <- getRootStore
        return (appState store)
    putAppState s = do
        store <- getRootStore
        putRootStore (store  { appState = s }) 

class Monad m => MonadGameState m where
    getGameState :: m GameState
    putGameState :: GameState -> m ()

instance (Monad m, MonadRootStore m) => MonadGameState  m where
    getGameState = do
        store <- getRootStore
        return (gameState store)
    putGameState s = do
        store <- getRootStore
        putRootStore (store  { gameState = s }) 

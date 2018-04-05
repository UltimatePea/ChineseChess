{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module AppState where

import Control.Monad.State.Lazy


data PieceType = General | Advisor | Elephant | Horse | Chariot | Cannon | Soldier | Empty deriving Eq
data PieceSide = Red | Black | None deriving Eq
data Piece = Piece PieceSide PieceType

data RawBoard a = RawBoard [[a]]
type Board = RawBoard Piece

data AppState = Normal | PieceSelected {
    selectedPosition :: (Int, Int),
    reachablePositions :: [(Int, Int)]
    } | End | AppError String | OperationSuccessful String

data RootStore = RootStore
    {
          cursorPosition :: (Int, Int)
        , board :: Board
        , appState :: AppState
        , movementAction :: ((Int, Int), (Int, Int))
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
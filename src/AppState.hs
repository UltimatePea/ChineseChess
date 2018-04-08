{-# LANGUAGE ConstraintKinds #-}
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
data AIAlgorithm = RandomDecision | MiniMax | MiniMaxWithAlphaBeta
data Handler = AIHandler AIAlgorithm PieceSide 


data HistoryRecord = HistoryRecord {
    historyFromPos :: (Int, Int)
    , historyToPos :: (Int, Int)
    , originalPieceAtToPos :: Piece
    }
data HistoryStack = HistoryStack {
      undoStack :: [HistoryRecord]
    , redoStack :: [HistoryRecord]
}

data RootStore =  RootStore
    {
          cursorPosition :: (Int, Int)
        , board :: Board
        , appState :: AppState
        , movementAction :: ((Int, Int), (Int, Int))
        , gameState :: GameState
        , historyStack :: HistoryStack
        , customHandlers ::  [Handler]
    }

type ApplicationMonad m = (Monad m, MonadRootStore' m,  MonadAppState' m, MonadGameState' m, MonadHistoryStack' m, MonadCustomHandlers' m)

class Monad m => MonadRootStore m where
    getRootStore :: m RootStore
    putRootStore :: RootStore -> m ()

type MonadRootStore' m = (Monad m, MonadState RootStore m)
instance (Monad m, MonadState RootStore m) => MonadRootStore  m where
    getRootStore = get
    putRootStore = put

class Monad m => MonadAppState m where
    getAppState :: m AppState
    putAppState :: AppState -> m ()

type MonadAppState' m = (Monad m, MonadRootStore' m)
instance (Monad m, MonadRootStore' m) => MonadAppState m where
    getAppState = do
        store <- getRootStore
        return (appState store)
    putAppState s = do
        store <- getRootStore
        putRootStore (store  { appState = s }) 

class Monad m => MonadGameState m where
    getGameState :: m GameState
    putGameState :: GameState -> m ()

type MonadGameState' m = (Monad m, MonadRootStore' m)
instance (Monad m, MonadRootStore' m) => MonadGameState  m where
    getGameState = do
        store <- getRootStore
        return (gameState store)
    putGameState s = do
        store <- getRootStore
        putRootStore (store  { gameState = s }) 

class Monad m => MonadBoard m where
    getBoard :: m Board
    putBoard :: Board -> m ()

type MonadBoard' m = (Monad m, MonadRootStore' m)
instance (Monad m, MonadRootStore' m) => MonadBoard m where
    getBoard = do
        store <- getRootStore
        return (board store)
    putBoard b = do
        store <- getRootStore
        putRootStore (store { board = b})


--- HISTORY STACK

class Monad m => MonadHistoryStack m where
    getHistoryStack :: m HistoryStack
    putHistoryStack :: HistoryStack -> m ()

    -- this method overwrites redo stack
    appendHistory :: HistoryRecord -> m ()
    appendHistory record = do
        stack <- getHistoryStack
        putHistoryStack $ HistoryStack {
            undoStack = record : undoStack stack
          , redoStack = []
        }
        
type MonadHistoryStack' m = (Monad m, MonadRootStore' m)
instance (Monad m, MonadRootStore' m) => MonadHistoryStack m where
    getHistoryStack = do
        store <- getRootStore
        return (historyStack store)
    putHistoryStack h = do
        store <- getRootStore
        putRootStore (store { historyStack = h})



class Monad m => MonadCustomHandlers m where
    getCustomHandlers :: m [Handler]
    putCustomHandlers :: [Handler] -> m ()

    addCustomHandler :: Handler -> m ()
    addCustomHandler h = do
        handlers <- getCustomHandlers
        putCustomHandlers (h:handlers)

    clearCustomHandlers :: m ()
    clearCustomHandlers = do
        putCustomHandlers []

type MonadCustomHandlers' m = (Monad m, MonadRootStore' m)
instance (Monad m, MonadRootStore' m) => MonadCustomHandlers m where
    getCustomHandlers = do
        store <- getRootStore
        return (customHandlers store)
    putCustomHandlers ch = do
        store <- getRootStore
        putRootStore (store { customHandlers = ch})

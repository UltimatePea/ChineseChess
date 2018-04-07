module HistorySystem where

import AppState
import CoreLib


undoRecord :: (MonadAppState' m, MonadHistoryStack' m, MonadBoard' m) =>  HistoryRecord -> m ()
undoRecord (HistoryRecord from@(fr, fc) to@(tr, tc) originalTo) = do
                originalFrom <- getPiece tr tc
                updatePiece fr fc originalFrom
                updatePiece tr tc originalTo

redoRecord :: (MonadAppState' m, MonadHistoryStack' m, MonadBoard' m) => HistoryRecord -> m ()
redoRecord (HistoryRecord from@(fr, fc) to@(tr, tc) _) = do
                piece <- getPiece fr fc
                updatePiece tr tc piece
                updatePiece fr fc (Piece None Empty)

-- Corresponds to actions
undo :: (MonadAppState' m, MonadHistoryStack' m, MonadBoard' m)  => m ()
undo = do
    getHistoryStack >>= \history ->
        case undoStack history of
            [] -> putAppState (AppError "No action to undo")
            (x:xs) -> do
                undoRecord x
                putHistoryStack $ HistoryStack {
                    undoStack = xs
                    , redoStack = (x:redoStack history)
                    }
                putAppState (OperationSuccessful $ "Undo Successful")

redo :: (MonadAppState' m, MonadHistoryStack' m, MonadBoard' m) => m ()
redo = do
    getHistoryStack >>= \history ->
        case redoStack history of
            [] -> putAppState (AppError "No action to redo")
            (x:xs) -> do
                redoRecord x
                putHistoryStack $ HistoryStack {
                    undoStack = (x:undoStack history)
                    , redoStack = xs
                    }
                putAppState (OperationSuccessful $ "Redo Successful")

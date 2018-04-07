module AI.Decisions where

import CoreLib
import ApplicationMonads
import AppState
import GameLogic
import AI.AIInterface

type Move = ((Int, Int), (Int, Int))
allViableMoves :: (AIMonad m) => PieceSide -> m [((Int, Int), (Int, Int))]
allViableMoves side = do
        -- get all positions
    let allPositions =  allBoardPositions 
        -- for each position, get list of destination positions
    listOfPairs <- 
        mapM (\fromPosition@(r,c) -> do
                (Piece fromSide t) <- getPiece r c
                if (fromSide /= side || t == Empty)
                    then return []
                    else do
                         putPosition fromPosition
                         -- TODO: CHANGE THIS PATTERN OF SAVING
                         saveState <- getGameState
                         putGameState (InGame side)
                         toPositions <- getAvailablePositions -- WARNING: WE MUST WRITE SIDE, BUG, we must write side since get available positions take side into account
                         putGameState saveState
                         return $ map (\t -> (fromPosition,t)) toPositions
                ) allPositions
    return $ concat listOfPairs
    
-- TODO: use a seperate decision space board instead of the global board
    
-- these are highly efficient decision execution algorithm, compared to generic MovePiece,
-- as there is no logging to history, no app state change etc
undoExecuteDecision :: (AIMonad m) => (Move, Piece) -> m ()
undoExecuteDecision (((fr,fc), (tr,tc)), originalToPiece) = do
    getPiece tr tc >>= updatePiece fr fc 
    updatePiece tr tc originalToPiece

-- returns an object which undoes the board when passed into undoExecuteDecision
executeDecision :: (AIMonad m) => Move -> m (Move, Piece) 
executeDecision m@((fr,fc), (tr,tc)) = do
    toPiece <- getPiece tr tc
    getPiece fr fc >>= updatePiece tr tc
    updatePiece fr fc (Piece None Empty)
    return (m, toPiece)
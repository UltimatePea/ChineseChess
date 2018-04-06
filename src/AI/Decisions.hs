module AI.Decisions where

import CoreLib
import ApplicationMonads
import AppState
import GameLogic
import AI.AIInterface

allMovablePieces :: (AIMonad m) => PieceSide -> m [((Int, Int), (Int, Int))]
allMovablePieces side = do
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
                         toPositions <- getAvailablePositions
                         return $ map (\t -> (fromPosition,t)) toPositions
                ) allPositions
    return $ concat listOfPairs
    
    

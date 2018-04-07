module AI.MiniMax where

import AI.AIInterface
import AI.Decisions
import AppState
import CoreLib
import Data.List
import Control.Monad.IO.Class


minimaxDefaultDepth :: Int
minimaxDefaultDepth = 2


runMiniMax :: (AIMonad m) => Int -> PieceSide -> m (Int, Move)
runMiniMax depth side = do
    -- evaluate choices to depth level 
    if depth == 0 -- TODO: OR mate checked
    then boardValue >>= \i -> return (i, ((0,0),(0,0)))
    else do
        choices <- allViableMoves side
        -- get all board values after applying each choice
        childrenValues <- flip mapM choices (\choice -> do
                undoable <- executeDecision choice
                (value, _) <- runMiniMax (depth - 1) (if side == Red then Black else Red)
                undoExecuteDecision undoable
                --if depth == minimaxDefaultDepth
                --    then liftIO $ putStrLn $ "Complete 1 Total " ++ show (length choices)
                --    else return ()
                return (value, choice)
            )
        return $ (if side == Red then maximumBy else minimumBy)
            (\(val1, _) (val2, _) -> compare val1 val2) childrenValues
        


boardValue :: (AIMonad m) => m Int
boardValue = mapM valueOfPieceAt allBoardPositions >>= return . sum -- return sum of all pieces
        

-- we need board to determine if the soldier has crossed the river
valueOfPieceAt :: (AIMonad m) => (Int, Int) -> m Int
valueOfPieceAt (r,c) = do
    piece <- getPiece r c
    return $ case piece of
        (Piece _ Empty) -> 0
        (Piece Red Soldier) -> if r <= 4 then 20 else 10
        (Piece Black Soldier) -> if r >= 5 then -20 else -10
        (Piece Red General) -> 1000
        (Piece Black General) -> -1000
        (Piece Red Advisor) -> 20
        (Piece Black Advisor ) -> -20
        (Piece Red Elephant) -> 20
        (Piece Black Elephant ) -> -20
        (Piece Red Horse) -> 40
        (Piece Black Horse ) -> -40
        (Piece Red Cannon) -> 40
        (Piece Black Cannon ) -> -40
        (Piece Red Chariot) -> 90
        (Piece Black Chariot ) -> -90 
    

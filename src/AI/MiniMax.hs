module AI.MiniMax where

import AI.AIInterface
import AI.Decisions
import AppState
import CoreLib
import Data.List
import Data.Foldable
import Control.Monad.IO.Class
import Debug.Trace


minimaxDefaultDepth :: Int
minimaxDefaultDepth = 3


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
        
-- implemented according to https://en.wikipedia.org/wiki/Alpha–beta_pruning            
runMiniMaxAlphaBeta :: (AIMonad m) => Int -> Int -> Int -> PieceSide -> m (Int, Move)
runMiniMaxAlphaBeta lowerBound upperBound depth side = do
    if depth == 0
        then boardValue >>= \i -> return (i, ((0,0),(0,0)))
        else do
            -- Note: We could not skip computing all choices, by checking parameters first
            -- since the recursive invocation ensures l < u
            choices <- allViableMoves side
            foldlM (\((curL, curU, value), move)  choice -> do -- value is for the purpose of return 

                if curL >= curU -- if lower bound is greater than or equal to upper bound,
                    then return ((curL, curU, value), move)
                    else do
                        -- run choice
                        undoable <- executeDecision choice
                        (value2, _) <- runMiniMaxAlphaBeta curL curU (depth - 1) (if side == Red then Black else Red)
                        undoExecuteDecision undoable

                        -- update curL or curU
                        let nextValue = (if side == Red then max curL value2 else min curU value2) 
                        let nextLower = (if side == Red then max curL nextValue else curL)-- update lower bound if we are red (max player)
                        let nextUpper = (if side == Black then min curU nextValue else curU)-- update upper bound if we are blck (min side)
                        let nextMove = if (side == Black && value2 < curU)
                                            || (side == Red && value2 > curL)
                                        then choice
                                        else move
                        --trace (" depth = " ++ show depth ++ " side = " ++ show side ++ 
                        --    " curL = " ++ show curL ++ " curU = " ++ show curU ++ " value = " ++ show value ++ " move = " ++ show move 
                        -- ++ " value2 = " ++ show value2 ++ " choice = " ++ show choice ++
                        -- " val = " ++ show nextValue ++ " nl = " ++ show nextLower ++ " nu = " ++ show nextUpper ++ " nm = " ++ show nextMove )
                        return ((nextLower,  nextUpper, nextValue), nextMove)

              ) ((lowerBound, upperBound, 
                            if side == Red then -10000000 else 10000000
              ), ((0,0),(0,0))) choices -- TODO: change default value
                            
                  >>= \((curL, curU, value), move) -> 
                    --trace ("returning curL = " ++ show curL ++ " curU = " ++ show curU ++ " value = " ++ show value ++ " move = " ++ show move)
                    return (value, move)



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
        (Piece Red Cannon) -> 45
        (Piece Black Cannon ) -> -45
        (Piece Red Chariot) -> 90
        (Piece Black Chariot ) -> -90 
    

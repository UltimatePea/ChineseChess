{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module GameLogic where

import AppState
import CoreLib
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

class Monad m => MonadMovePieceAction m where
    getMoveFrom :: m (Int, Int)
    getMoveTo :: m (Int, Int)
    putMoveAction :: (Int, Int) -> (Int, Int) -> m ()

 -- WARNING: ARCHITECTURAL WARNING: move action should only be set when the user is moving, therefore should not appear in root store
instance (Monad m, MonadRootStore m) => MonadMovePieceAction m where
    getMoveFrom = do
        store <- getRootStore
        let (from , _) = movementAction store
        return from
    getMoveTo = do
        store <- getRootStore
        let (_ , to) = movementAction store
        return to
    putMoveAction from to = do
        store <- getRootStore
        putRootStore (store {movementAction = (from, to)})


movePiece :: (MonadBoard m, MonadMovePieceAction m) =>  EitherT String m ()
movePiece = do
    from@(fromRow, fromCol) <- lift getMoveFrom
    to@(toRow, toCol) <- lift getMoveTo
    (Piece fromredblack fromtype) <- lift $ getPiece fromRow fromCol
    (Piece toredblack totype) <- lift $ getPiece toRow toCol
    -- since we're in the eitherT monad (similar to exceptT), if check fails, the below computation will not execute
    checkMoveViability
    lift $ updatePiece fromRow fromCol (Piece None Empty)
    lift $ updatePiece toRow toCol (Piece fromredblack fromtype)

checkMoveViability :: (MonadBoard m, MonadMovePieceAction m ) =>  EitherT String m ()
checkMoveViability  = do
    from@(fromRow, fromCol) <- lift getMoveFrom
    to@(toRow, toCol) <- lift getMoveTo
    p@(Piece fromredblack fromtype) <- lift $ getPiece fromRow fromCol
    (Piece toredblack totype) <- lift $ getPiece toRow toCol
    if fromtype == Empty 
    then left $ "There is no movable piece at (" ++ show fromRow ++ ", " ++ show fromCol ++ ")"
    else if fromredblack == toredblack 
         then left $ "Cannot move piece onto a piece on the same side, or itself"
         else -- we should do other condition checks, 
              checkPieceSpecificMove p from to

class PieceSpecificCheck p where
    -- this only  checks piece specific moves (precondition : from is not empty, to is reachable)
    checkPieceSpecificMove :: MonadBoard m => p ->  (Int, Int) -> (Int, Int) -> EitherT String m ()

-- the distance is h + w, not straight line distance
-- Move to Right / Bottom : Plus. Move to Left/Top : Minus
data MoveType = Move {direction :: MoveDirection, distance :: Int, hUnits :: Int, vUnits :: Int}
data MoveDirection = Horizontal | Vertical | Diagonal | Other deriving Eq

getMoveType :: (Int, Int) -> (Int, Int) -> MoveType
getMoveType from@(fromRow, fromCol) to@(toRow, toCol)
    | fromRow == toRow = let d = toCol - fromCol
                         in Move Horizontal (abs d) d 0
    | fromCol == toCol = let d = toRow - fromRow
                         in Move Vertical (abs d) 0 d
    | otherwise = let  dc = toCol - fromCol
                       dr = toRow - fromRow
                  in if abs dc == abs dr
                        then Move Diagonal (abs dc) dc dr
                        else Move Other (abs dc + abs dr) dc dr

instance PieceSpecificCheck Piece where
    checkPieceSpecificMove (Piece fromredblack fromtype) from@(fromRow, fromCol) to@(toRow, toCol)
        | fromtype == General   -- for general check to whether to position is inside the 3x3 square
            = if (not (fromredblack == Red && toRow >= 7 && toRow <= 9 && toCol >= 3 && toCol <= 5)) &&
                (not (fromredblack == Black && toRow >= 0 && toRow <= 2 && toCol >= 3 && toCol <= 5))
              then left $ "The general must reside in 3x3 square"
              else if distance (getMoveType from to ) /= 1
                   then left $ "The general must move only one unit horizontally or vertically"
                   else right ()
        | fromtype == Advisor   -- for general check to whether to position is inside the 3x3 square
            = if (not (fromredblack == Red && toRow >= 7 && toRow <= 9 && toCol >= 3 && toCol <= 5)) &&
                (not (fromredblack == Black && toRow >= 0 && toRow <= 2 && toCol >= 3 && toCol <= 5))
              then left $ "The advisor must reside in 3x3 square"
              else let movetype = getMoveType from to 
                   in if distance movetype /= 1 || direction movetype /= Diagonal
                   then left $ "The advisor must move only one unit diagonally"
                   else right ()

            
                

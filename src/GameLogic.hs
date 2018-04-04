module GameLogic where

import AppState
import CoreLib

movePiece :: MonadBoard m => (Int, Int) -> (Int, Int) -> m (Either String ())
movePiece (fromRow, fromCol) (toRow, toCol) = do
    (Piece fromredblack fromtype) <- getPiece fromRow fromCol
    (Piece toredblack totype) <- getPiece toRow toCol
    if fromtype == Empty 
    then return (Left $ "There is no movable piece at (" ++ fromRow ++ ", " ++ fromCol ++ ")")
    else if fromredblack == toredblack 
         then return (Left $ "Cannot move piece onto a piece on the same side")
         else -- we should do other condition checks, 
            do
                updatePiece fromRow fromCol (Piece Red Empty)
                updatePiece toRow toCol (Piece fromredblack fromtype)
{-# LANGUAGE LambdaCase #-}

module GameLogic where

import AppState
import CoreLib
import ApplicationMonads
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad


-- asserts a boolean value, if false, invalidate either T monad
assertThat :: (Monad m) => Bool -> String -> EitherT String m ()
assertThat b str 
        | not b = left str
        | b = right ()

assertThat' :: (Monad m) => String -> Bool -> EitherT String m ()
assertThat' = flip assertThat

-- This method must be called upon the initiation of a piece selected, the movable piece must be the piece that's under the cursor(position in position monad)
getAvailablePositions :: (MonadBoard' m, MonadMovePieceAction' m, MonadAppState' m, MonadPosition' m, MonadGameState' m) => m [(Int, Int)]
getAvailablePositions = do
    (r,c) <- getPosition 
    res <- flip filterM allBoardPositions $ \(r2, c2) -> do
        putMoveAction (r,c) (r2,c2)
        res <- runEitherT checkMoveViability
        case res of
            Left _ -> return False
            Right _ -> return True
    return res

executeMove :: (MonadBoard' m, MonadMovePieceAction' m, MonadGameState' m, MonadAppState' m, MonadHistoryStack' m) => Bool  -- whether we should write to history
                                -> ((Int, Int), (Int, Int)) -> m ()
executeMove writeHistory ((r,c),(r2,c2)) = do
            putMoveAction (r,c) (r2,c2)
            moveres <- runEitherT movePiece 
            case moveres of
                Right pieceTaken -> do
                    if writeHistory
                        then appendHistory (HistoryRecord (r,c) (r2,c2) pieceTaken)
                        else return () -- do nothing on else
                    putAppState (OperationSuccessful "Piece Moved")
                Left reason -> putAppState (AppError reason)

-- failable
movePiece :: (MonadBoard' m, MonadMovePieceAction' m, MonadGameState' m) =>  EitherT String m Piece -- returns the piece taken
movePiece = do
    from@(fromRow, fromCol) <- lift getMoveFrom
    to@(toRow, toCol) <- lift getMoveTo
    (Piece fromredblack fromtype) <- lift $ getPiece fromRow fromCol
    taken@(Piece toredblack totype) <- lift $ getPiece toRow toCol
    -- since we're in the eitherT monad (similar to exceptT), if check fails, the below computation will not execute
    checkMoveViability
    lift $ updatePiece fromRow fromCol (Piece None Empty)
    lift $ updatePiece toRow toCol (Piece fromredblack fromtype)
    -- update game state accordingly

    currentGameState <- lift getGameState 
    lift $ putGameState $ case currentGameState of
        NotInGame -> NotInGame
        GameFinished x -> GameFinished x
        InGame x -> 
            -- game finished if general is taken
            if totype == General 
            then GameFinished x
            else InGame $ if x == Red then Black else Red

    return taken

checkMoveViability :: (MonadBoard' m, MonadMovePieceAction' m, MonadGameState' m ) =>  EitherT String m ()
checkMoveViability  = do
    from@(fromRow, fromCol) <- lift getMoveFrom
    to@(toRow, toCol) <- lift getMoveTo
    p@(Piece fromredblack fromtype) <- lift $ getPiece fromRow fromCol
    (Piece toredblack totype) <- lift $ getPiece toRow toCol

    -- check when in game
    lift getGameState >>= \case
        NotInGame -> return ()
        GameFinished _ -> return ()
        InGame side -> assertThat (fromredblack == side) ("Move only " ++ show side ++ "'s pieces")


    assertThat (fromtype /= Empty ) 
        $ "There is no movable piece at (" ++ show fromRow ++ ", " ++ show fromCol ++ ")"
    assertThat (fromredblack /= toredblack) 
        $ "Cannot move piece onto a piece on the same side, or itself"
    -- we should do other condition checks, 
    checkPieceSpecificMove p from to

class PieceSpecificCheck p where
    -- this only  checks piece specific moves (precondition : from is not empty, to is reachable)
    checkPieceSpecificMove :: MonadBoard' m => p ->  (Int, Int) -> (Int, Int) -> EitherT String m ()

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
unit :: Int -> Int
unit x  | x > 0 = 1
        | x < 0 = -1


checkNonBlocking :: (MonadBoard' m) => (Int, Int) -> (Int, Int) -> EitherT String m ()
checkNonBlocking from@(fromRow, fromCol) to@(toRow, toCol) = do
                
                let movetype = getMoveType from to 
                let coordinates = case direction movetype of 
                     Horizontal -> flip map (tail $ init [fromCol, fromCol+(unit (hUnits movetype)) .. toCol])
                        $ \x -> (fromRow, x)
                     Vertical -> flip map (tail $ init [fromRow, fromRow+(unit (vUnits movetype)) .. toRow])
                        $ \x -> (x, fromCol)
                -- get intermediate coordinates
                empties <- sequence $ (flip map  coordinates
                    $ \(r,c) -> (lift $ checkEmpty r c ))

                -- assert that all intermediates should be true
                assertThat' "There should be no pieces in between" (and empties ) 


instance PieceSpecificCheck Piece where
    -- checkPieceSpecificMove :: MonadBoard m => Piece ->  (Int, Int) -> (Int, Int) -> EitherT String m ()
    checkPieceSpecificMove (Piece fromredblack fromtype) from@(fromRow, fromCol) to@(toRow, toCol)

        | fromtype == General   -- for general check to whether to position is inside the 3x3 square
            = do
                (Piece toredblack totype) <- lift $ getPiece toRow toCol
                if toredblack /= fromredblack && totype == General
                    then do
                        let movetype = getMoveType from to 
                        assertThat (direction movetype == Vertical)
                                $ "The general vs general must move vertically"
                        -- check that nothing is blocking in the way
                        checkNonBlocking from to
                else do
                    assertThat ((fromredblack == Red && toRow >= 7 && toRow <= 9 && toCol >= 3 && toCol <= 5)
                        || (fromredblack == Black && toRow >= 0 && toRow <= 2 && toCol >= 3 && toCol <= 5))
                        $  "The general must reside in 3x3 square"
                    let movetype = (getMoveType from to )
                    assertThat (distance movetype == 1 && 
                        (direction movetype == Horizontal || direction movetype == Vertical))
                            $ "The general must move only one unit horizontally or vertically"

        | fromtype == Advisor   -- for general check to whether to position is inside the 3x3 square
            = do
                assertThat ((fromredblack == Red && toRow >= 7 && toRow <= 9 && toCol >= 3 && toCol <= 5)
                    || (fromredblack == Black && toRow >= 0 && toRow <= 2 && toCol >= 3 && toCol <= 5))
                        $ "The advisor must reside in 3x3 square"
                let movetype = getMoveType from to 
                assertThat (distance movetype == 1 && direction movetype == Diagonal)
                        $ "The advisor must move only one unit diagonally"

        | fromtype == Horse
            = do
                let movetype = getMoveType from to 
                assertThat (distance movetype == 3 && direction movetype == Other)
                    $ "Horse must move in 2x1 grid"
                (if abs (hUnits movetype)== 2 
                     then checkEmpty' fromRow (fromCol + unit (hUnits movetype)) 
                     else checkEmpty' (fromRow + unit (vUnits movetype)) fromCol)
                    >>= assertThat' "Horse's leg must not be hobbled"
                
        | fromtype == Elephant   
            = do
                assertThat ( (fromredblack == Red && toRow >= 5)
                    || (fromredblack == Black && toRow <= 4 ))
                        $ "The elephant may not cross the river"
                let movetype = getMoveType from to 
                assertThat (distance movetype == 2 && direction movetype == Diagonal)
                        $ "The elephant must move only two unit diagonally"
                checkEmpty' (fromRow + unit (vUnits movetype)) (fromCol + unit (hUnits movetype))
                    >>= assertThat' "The elephant's eye must not be blocked"

        | fromtype == Chariot
            = do
                let movetype = getMoveType from to 
                assertThat (direction movetype == Vertical || direction movetype == Horizontal)
                        $ "The chariot must move horizontally or verticaly"
                -- check that nothing is blocking in the way
                checkNonBlocking from to

        -- WARNING: copying from above
        | fromtype == Cannon
            = do
                let movetype = getMoveType from to 
                assertThat (direction movetype == Vertical || direction movetype == Horizontal)
                        $ "The Cannon must move horizontally or verticaly"
                -- check that nothing is blocking in the way
                
                -- get intermediate coordinates
                let coordinates = if direction movetype == Horizontal
                    then flip map (tail $ init [fromCol, fromCol+(unit (hUnits movetype)) .. toCol])
                        $ \x -> (fromRow, x)
                    else flip map (tail $ init [fromRow, fromRow+(unit (vUnits movetype)) .. toRow])
                        $ \x -> (x, fromCol)
                empties <- sequence $ flip map  coordinates
                   
                    $ \(r,c) -> lift $ checkEmpty r c 

                -- assert that all intermediates should be true
                let numberOfPiecesInBetween = (sum $ map (\b -> if b then 0 else 1) empties)
                (Piece _ totype) <- lift $ getPiece toRow toCol
                assertThat' "There should be zero or one piece in between" 
                    (if totype == Empty then  numberOfPiecesInBetween == 0
                         else numberOfPiecesInBetween == 1 )
        
        | fromtype == Soldier
            = do
                let movetype = (getMoveType from to )
                assertThat (distance movetype == 1 && 
                    (direction movetype == Horizontal || direction movetype == Vertical))
                    "The soldier must move onely one unit"
                assertThat (
                    (if fromredblack == Red && fromRow >= 5 then direction movetype == Vertical else True) 
                    &&
                    (if fromredblack == Black && fromRow <= 4 then direction movetype == Vertical else True)
                    ) "The soldier must move forward before corssing the river"
                assertThat (
                    (fromredblack == Red && vUnits movetype <= 0)
                    || 
                    (fromredblack == Black && vUnits movetype >= 0)
                    ) "The soldier must never move backwards"



            
                

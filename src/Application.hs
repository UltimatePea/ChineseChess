{-# LANGUAGE LambdaCase #-}
module Application where

import CoreLib
import AppState
import Control.Monad.State.Lazy
import Control.Monad.Trans.Either
import ApplicationDeclaration
import Printing
import GameLogic
import CommandSystem
import ApplicationMonads
import CursorInputControl
import System.Console.ANSI
import System.IO




entry :: IO ()
entry = 
    let st1 = evalStateT mainApp (RootStore (0, 0) (RawBoard []) 
            Normal ((0, 0), (0,0)) NotInGame 
            )
    in st1

mainApp :: Application
mainApp = do 
    putInitialGameBoard
    liftIO clearScreen
    controlMainLoop



controlMainLoop :: Application
controlMainLoop = do

    -- 2. Set Correct Cursor Position -- We Use Virtual Cursors

    -- 1. Print Current Board
    -- clear board first
    -- liftIO clearScreen
    -- hide cursor so users don't see flashes
    liftIO hideCursor

    -- first move the cursor to upper left corner
    liftIO $ setCursorPosition 0 0
    -- then print the board
    colorPrintBoard 
    -- then print other info after clearing info
    liftIO $ clearFromCursorToScreenEnd
    printPositionInfo
    printGameState
    printAppState

    -- we can always leave the cursor at tend of our print
    -- commented out as two cursors cause confusions
    -- liftIO showCursor





    -- 3. Listen for user inputs
    -- push any pending output
    liftIO $ hFlush stdout
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ hSetBuffering stdin NoBuffering
    -- listen for input
    isEof <- liftIO $ hIsEOF stdin
    if isEof -- also check hReady, e.g. up arrow is esc[a
    then do
        liftIO showCursor
        return ()
    else do
        c <- liftIO $ hGetChar stdin
        handleDirect c

        -- do app state check here
        state <- getAppState
        case state of 
            End -> liftIO showCursor >> return ()
            _ -> controlMainLoop


handleDirect :: Char -> Application
handleDirect ':' = do
    liftIO $ hSetBuffering stdin LineBuffering
    liftIO showCursor
    str <- liftIO getLine
    handleCommand str
    liftIO hideCursor
    liftIO $ hSetBuffering stdin NoBuffering


handleDirect 'h' = moveCursor MoveLeft >> postMoveStateChange
handleDirect 'j' = moveCursor MoveDown >> postMoveStateChange
handleDirect 'k' = moveCursor MoveUp >> postMoveStateChange
handleDirect 'l' = moveCursor MoveRight >> postMoveStateChange
handleDirect '\EOT' = putAppState End
handleDirect 'm' = do
    state <- getAppState
    case state of
        _ -> do
            (r,c) <- getPosition
            availablePos <- getAvailablePositions
            -- only select if the piece is movable
            if availablePos /= []
            then putAppState (PieceSelected (r,c) availablePos)
            else putAppState (AppError "You cannot move this piece")
handleDirect 'y' = do
    state <- getAppState
    case state of 
        PieceSelected (r,c) _ -> do
            (r2, c2) <- getPosition
            putMoveAction (r,c) (r2,c2)
            moveres <- runEitherT  movePiece 
            case moveres of
                Right _ -> putAppState (OperationSuccessful "Piece Moved")
                Left reason -> putAppState (AppError reason)
        _ -> putAppState (AppError "y not available, use m to select a piece first")

handleDirect '\ESC' = putAppState Normal
handleDirect 'n' = do
    state <- getAppState
    case state of 
        PieceSelected _ _ -> do
                putAppState (OperationSuccessful "Move Cancelled")
        _ -> putAppState (AppError "n not available, use m to select a piece first")
handleDirect x = putAppState (AppError $ "Unrecognized operation " ++ show x)

-- This method must be called upon the initiation of a piece selected, the movable piece must be the piece that's under the cursor
getAvailablePositions :: (MonadBoard m, MonadMovePieceAction m, MonadAppState m, MonadPosition m, MonadGameState m) => m [(Int, Int)]
getAvailablePositions = do
    (r,c) <- getPosition 
    res <- flip filterM allBoardPositions $ \(r2, c2) -> do
        putMoveAction (r,c) (r2,c2)
        res <- runEitherT checkMoveViability
        case res of
            Left _ -> return False
            Right _ -> return True
    return res

    
        


postMoveStateChange :: (MonadAppState m) => m ()
postMoveStateChange = do
    state <- getAppState
    case state of
        OperationSuccessful _ -> putAppState Normal
        AppError _ -> putAppState Normal
        _ -> return ()

printAppState :: (MonadAppState m, MonadIO m) => m ()
printAppState = do
    state <- getAppState
    case state of
        Normal -> 
              liftIO $ putStrLn "Press hjkl to move cursor, m to move a piece"
        PieceSelected _ _ -> liftIO $ putStrLn "Press hjkl to move cursor, y to confirm, n to cancel"
        AppError reason -> liftIO $ putStrLn $ "Error: " ++ reason
        OperationSuccessful status -> liftIO $ putStrLn $ "OK: " ++ status
        -- End not happending here


printGameState :: (MonadGameState m, MonadIO m) => m ()
printGameState = do
    state <- getGameState
    case state of
        NotInGame -> 
              liftIO $ putStrLn "Currently no game, type :begin game to start a game"
        InGame side -> liftIO $ putStrLn $ "In game, now " ++ show side ++ " moves"
        GameFinished side -> liftIO $ putStrLn $ "Game finished, " ++ show side ++ " won"
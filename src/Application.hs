module Application where

import CoreLib
import AppState
import Control.Monad.State.Lazy
import Control.Monad.Trans.Either
import ApplicationDeclaration
import Printing
import GameLogic
import CursorInputControl
import System.Console.ANSI
import System.IO




entry :: IO ()
entry = 
    let st1 = evalStateT mainApp (RootStore (0, 0) (RawBoard []) Normal ((0, 0), (0,0)))
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
        handle c

        -- do app state check here
        state <- getAppState
        case state of 
            End -> liftIO showCursor >> return ()
            _ -> controlMainLoop

handle :: Char -> Application
handle 'h' = moveCursor MoveLeft >> postMoveStateChange
handle 'j' = moveCursor MoveDown >> postMoveStateChange
handle 'k' = moveCursor MoveUp >> postMoveStateChange
handle 'l' = moveCursor MoveRight >> postMoveStateChange
handle '\EOT' = putAppState End
handle 'm' = do
    state <- getAppState
    case state of
        _ -> do
            (r,c) <- getPosition
            putAppState (PieceSelected (r,c))
handle 'y' = do
    state <- getAppState
    case state of 
        PieceSelected (r,c) -> do
            (r2, c2) <- getPosition
            putMoveAction (r,c) (r2,c2)
            moveres <- runEitherT  movePiece 
            case moveres of
                Right _ -> putAppState (OperationSuccessful "Piece Moved")
                Left reason -> putAppState (AppError reason)
        _ -> putAppState (AppError "y not available, use m to select a piece first")

handle 'n' = do
    state <- getAppState
    case state of 
        PieceSelected _ -> do
                putAppState (OperationSuccessful "Move Cancelled")
        _ -> putAppState (AppError "n not available, use m to select a piece first")
handle x = putAppState (AppError $ "Unrecognized operation " ++ show x)

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
        Normal -> liftIO $ putStrLn "Press hjkl to move cursor, m to move a piece"
        PieceSelected _ -> liftIO $ putStrLn "Press hjkl to move cursor, y to confirm, n to cancel"
        AppError reason -> liftIO $ putStrLn $ "Error: " ++ reason
        OperationSuccessful status -> liftIO $ putStrLn $ "OK: " ++ status
        -- End not happending here
module Application where

import CoreLib
import Control.Monad.State.Lazy
import ApplicationDeclaration
import Printing
import CursorInputControl
import System.Console.ANSI
import System.IO




entry :: IO ()
entry = 
    let st1 = evalStateT mainApp (0, 0)
        st2 = evalStateT st1 newGameBoard
    in st2

mainApp :: Application
mainApp = do 
    controlMainLoop



controlMainLoop :: Application
controlMainLoop = do

    -- 2. Set Correct Cursor Position -- We Use Virtual Cursors


    -- 1. Print Current Board
    -- clear board first
    liftIO clearScreen
    -- hide cursor so users don't see flashes
    liftIO hideCursor

    -- first move the cursor to upper left corner
    liftIO $ setCursorPosition 0 0
    -- then print the board
    colorPrintBoard 
    -- then print other info
    printPositionInfo





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
        controlMainLoop

handle :: Char -> Application
handle 'h' = moveCursor MoveLeft
handle 'j' = moveCursor MoveDown
handle 'k' = moveCursor MoveUp
handle 'l' = moveCursor MoveRight
handle '\EOT' = moveCursor MoveRight
handle x = error $ "Unrecognized Character " ++ show x

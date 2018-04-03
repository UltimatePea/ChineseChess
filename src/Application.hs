module Application where

import CoreLib
import Control.Monad.State.Lazy
import ApplicationDeclaration
import Printing
import CursorInputControl
import System.Console.ANSI




entry :: IO ()
entry = 
    let st1 = evalStateT mainApp (0, 0)
        st2 = evalStateT st1 newGameBoard
    in st2

mainApp :: Application
mainApp = do 
    liftIO clearScreen
    liftIO $ setCursorPosition 0 0
    liftIO $ colorPrintBoard newGameBoard
    controlMainLoop

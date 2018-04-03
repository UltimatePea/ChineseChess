module Main where

import CoreLib
import Printing
import CursorInputControl
import System.Console.ANSI


main :: IO ()
main = do
    clearScreen
    setCursorPosition 0 0
    colorPrintBoard newGameBoard
    controlMainLoop

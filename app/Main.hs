module Main where

import CoreLib
import Printing
import System.Console.ANSI

main :: IO ()
main = do
    clearScreen
    setCursorPosition 0 0
    colorPrintBoard newGameBoard

module Printing where
import AppState
import CoreLib
import ApplicationMonads
import CursorInputControl
import Data.List (intersperse)
import qualified System.Console.ANSI as C
import Control.Monad.IO.Class

class Printable p where
    toPrintable :: p -> Char

class Colorable c where
    toColor  :: c -> (C.ColorIntensity , C.Color)

instance Colorable Piece where
    toColor (Piece _ Empty ) = (C.Dull, C.Yellow)
    toColor (Piece Red _ ) = (C.Vivid, C.Red)
    toColor (Piece Black _) = (C.Dull, C.Black)

instance Printable Piece where
    toPrintable (Piece Red General) = '帅'
    toPrintable (Piece Black General) = '将'
    toPrintable (Piece Red Advisor) = '仕'
    toPrintable (Piece Black Advisor) = '士'
    toPrintable (Piece Red Elephant) = '相'
    toPrintable (Piece Black Elephant) = '象'
    toPrintable (Piece _ Horse) = '马'
    toPrintable (Piece _ Chariot) = '车'
    toPrintable (Piece _ Cannon) = '炮'
    toPrintable (Piece Red Soldier) = '兵'
    toPrintable (Piece Black Soldier) = '卒'
    toPrintable (Piece _ Empty) = fullWidth '+'

        
data BoardCue = Space | HorizontalBar | VerticleBar 

instance Colorable BoardCue where
    toColor _ = (C.Vivid, C.Blue)

instance Printable BoardCue where
    toPrintable Space = fullWidth ' '
    toPrintable HorizontalBar = fullWidth '-'
    toPrintable VerticleBar = fullWidth '|'

fullWidth c = case c of
        ' ' -> '　'
        '-' -> '－'
        '+' -> '＋'
        '|' -> '｜'

colorPrint :: (Colorable a, Printable a) => a -> IO ()
colorPrint x = do
    let color = toColor x
    C.setSGR [C.SetColor C.Foreground (fst color) (snd color)]
    putStr $ (toPrintable x) : []
    C.setSGR [C.Reset]

colorPrintStr :: (Colorable a, Printable a) => [a] -> IO ()
colorPrintStr = mapM_ colorPrint

colorPrintStrLn :: (Colorable a, Printable a) => [a] -> IO ()
colorPrintStrLn xs = do
    colorPrintStr xs
    putStrLn ""

type PrintMonad m = (MonadBoard' m, MonadIO m, MonadPosition' m, MonadAppState' m, MonadBoard' m, MonadMovePieceAction' m, MonadHistoryStack' m)

    -- assumes non empty board, assume one division at y = 5
colorPrintBoard ::  PrintMonad m => m ()
colorPrintBoard  = getBoard >>= \(RawBoard xxs) -> 
    let width = length xxs
        height = length (head xxs) 
        xs = take 5 xxs -- first five rows
        ys = drop 5 xxs -- remaining rows
    in  if width == 0 -- this should shortcut the evaluation of height above
        then error "Cannot print empty board"
        else do
            sequence_ $ intersperse (liftIO $ printDivider width) (zipWith printRow [0..] xs) 
            liftIO $ printMiddleDivider width
            sequence_ $ intersperse (liftIO $ printDivider width) (zipWith printRow [5..] ys) 
printDivider :: Int -> IO ()
printDivider width = colorPrintStrLn (intersperse Space (replicate (width-1) VerticleBar) )
printMiddleDivider :: Int -> IO ()
printMiddleDivider width = colorPrintStrLn ( 
        [VerticleBar] ++ (replicate (2 * (width-1) - 3) Space) ++ [VerticleBar])
printRow :: (Colorable a, Printable a,  PrintMonad m) => Int ->  [a] -> m ()
printRow rowNum row = sequence_ (intersperse (liftIO $ colorPrint HorizontalBar) (zipWith (printCell rowNum) [0..] row) )
                                        >> (liftIO $ putStrLn "")
printCell :: (Colorable a, Printable a, PrintMonad m) => Int -> Int ->  a -> m ()
printCell rowNum colNum c = do


    -- print hints of last move, this should appear on the bottom
    getHistoryStack >>= \history -> 
        case undoStack history of 
            [] -> return () -- no history yet
            (h:_) -> 
                let f@(fromR, fromC) =  historyFromPos h
                    t@(toR, toC) = historyToPos h
                in if (rowNum, colNum) == f || (rowNum, colNum) == t
                    then liftIO $ C.setSGR [C.SetColor C.Background C.Dull C.Cyan]
                    else return ()


    -- color piece selected with cyan bg, available positions with green, available attacking position with red
    getAppState >>= \state ->
        case state of
            PieceSelected p@(i,j) reachable -> 
                                   if rowNum == i && colNum == j
                                   then liftIO $ C.setSGR [C.SetColor C.Background C.Vivid C.Cyan]
                                   else if (rowNum,colNum) `elem` reachable 
                                        then checkEmpty rowNum colNum >>=  \b -> if b
                                             then liftIO $ C.setSGR [C.SetColor C.Background C.Dull C.Green]
                                             else liftIO $ C.setSGR [C.SetColor C.Background C.Vivid C.Green]
                                        else return ()

            _ -> return ()


            



    -- color cursor bg
    (i,j) <- getPosition
    if rowNum == i && colNum == j
        then liftIO $ C.setSGR [C.SetColor C.Background C.Vivid C.Black]
        else return ()
    liftIO $ colorPrint c



            


    
    
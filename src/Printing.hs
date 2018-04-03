module Printing where
import CoreLib
import Data.List (intersperse)
import qualified System.Console.ANSI as C

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

colorPrintStr :: (Colorable a, Printable a) => [a] -> IO ()
colorPrintStr = mapM_ colorPrint

colorPrintStrLn :: (Colorable a, Printable a) => [a] -> IO ()
colorPrintStrLn xs = do
    colorPrintStr xs
    putStrLn ""

    -- assumes non empty board, assume one division at y = 5
colorPrintBoard :: (Colorable a, Printable a) => RawBoard a -> IO ()
colorPrintBoard (RawBoard xxs) = 
    let width = length xxs
        height = length (head xxs) 
        xs = take 5 xxs -- first five rows
        ys = drop 5 xxs -- remaining rows
    in  if width == 0 -- this should shortcut the evaluation of height above
        then error "Cannot print empty board"
        else do
            sequence_ $ intersperse (printDivider width) (map printRow xs) 
            printMiddleDivider width
            sequence_ $ intersperse (printDivider width) (map printRow ys) 
        where   printDivider :: Int -> IO ()
                printDivider width = colorPrintStrLn (intersperse Space (replicate (width-1) VerticleBar) )
                printMiddleDivider :: Int -> IO ()
                printMiddleDivider width = colorPrintStrLn ( 
                    [VerticleBar] ++ (replicate (2 * (width-1) - 3) Space) ++ [VerticleBar])
                printRow :: (Colorable a, Printable a) => [a] -> IO ()
                printRow row = sequence_ (intersperse (colorPrint HorizontalBar) (map colorPrint row) )
                                >> putStrLn ""




            


    
    
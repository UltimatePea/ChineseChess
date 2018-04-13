module CommandSystem where
    
import AppState
import ChineseChess.CoreLib
import ChineseChess.GameLogic
import ApplicationMonads
import Data.List
import AI.AIInterface
import AI.RandomDecision
import AI.MiniMax
import AI.AIAlgorithm
import AIHandlerLogic
import Control.Monad.IO.Class
import System.Console.ANSI (clearScreen)


   


handleCommand :: (MonadAppState' m, MonadGameState' m, MonadBoard' m, MonadIO m, MonadHistoryStack' m, MonadCustomHandlers' m ) => String -> m ()
handleCommand "quit" = putAppState End
handleCommand "ai random" = checkStateAndRunAI RandomDecision 
handleCommand "ai minimax" = checkStateAndRunAI MiniMax
handleCommand "ai minimaxab" = checkStateAndRunAI MiniMaxWithAlphaBeta

handleCommand "installai" = installAI
handleCommand "clearai" = clearCustomHandlers  >> (putAppState (OperationSuccessful "AI disabled"))

handleCommand "reset board" = putInitialGameBoard  >> putHistoryStack (HistoryStack [] []) >> putAppState (OperationSuccessful "Board has been reset")
handleCommand "begin game" = putGameState (InGame Red) >> putAppState (OperationSuccessful "Game has begun")
handleCommand "redraw" = liftIO $ clearScreen
handleCommand "help" = putAppState (OperationSuccessful $ concat $ intersperse "\n"
        ["Available Command: "
        ,"begin game -- Start a game from current board state"
        ,"reset board -- Reset the board to initial state"
        , "ai random -- Make one move for current side with random algorithm"
        , "ai minimax -- Make one move for current side with Minimax algorithm"
        , "ai minimaxab -- Make one move for current side with Minimax algorithm with alpha beta pruning"
        , "clearai -- remove all installed ai"
        , "installai -- add an automatic ai for a side, similar effect of calling ai xxxx for that side"
        , "redraw -- repaints the screen, in case graphics go wrong"
        , "quit -- quit the program"
        ])
handleCommand cmd = putAppState (AppError $ "Unrecognized Command: " ++ cmd ++ "; type help to show commands")

installAI :: (MonadAppState' m, MonadIO m, MonadCustomHandlers' m) => m ()
installAI = do
        
        liftIO $ putStrLn "Which AI [minimax/minimaxab/random]? "
        algorithm <- liftIO getLine >>= \case
                        "minimax" -> return MiniMax
                        "minimaxab" -> return MiniMaxWithAlphaBeta
                        "random" -> return RandomDecision
        liftIO $ putStrLn "Which side [red/black]? "
        side <- liftIO getLine >>= \case
                        "red" -> return Red
                        "black" -> return Black
        addCustomHandler (AIHandler algorithm side)
        putAppState (OperationSuccessful "AI algorithm installed")
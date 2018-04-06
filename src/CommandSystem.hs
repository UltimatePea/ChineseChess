module CommandSystem where
    
import AppState
import CoreLib
import GameLogic
import ApplicationMonads
import Data.List
import AI.AIInterface
import AI.RandomDecision
import Control.Monad.IO.Class
import System.Console.ANSI (clearScreen)


checkStateAndRunAI :: (AIMonad m, AIDecider d) => d -> m ()
checkStateAndRunAI ai = do
        -- save cursor position
        cursor <- getPosition
        gameState <- getGameState
        case gameState of
                InGame side -> runAI ai side >>= executeMove True
                _ -> putAppState (AppError "Not in game")
        putPosition cursor
   


handleCommand :: (MonadAppState' m, MonadGameState' m, MonadBoard' m, MonadIO m) => String -> m ()
handleCommand "quit" = putAppState End
handleCommand "ai random" = checkStateAndRunAI RandomDecision 
handleCommand "reset board" = putInitialGameBoard >> putAppState (OperationSuccessful "Board has been reset")
handleCommand "begin game" = putGameState (InGame Red) >> putAppState (OperationSuccessful "Game has begun")
handleCommand "redraw" = liftIO $ clearScreen
handleCommand "help" = putAppState (OperationSuccessful $ concat $ intersperse "\n"
        ["Available Command: "
        ,"begin game"
        ,"reset board"
        , "ai random"
        , "redraw"
        , "quit"
        ])
handleCommand cmd = putAppState (AppError $ "Unrecognized Command: " ++ cmd ++ "; type help to show commands")
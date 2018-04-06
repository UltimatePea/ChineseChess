module CommandSystem where
    
import AppState
import CoreLib
import GameLogic
import Data.List
import AI.AIInterface
import AI.RandomDecision
import Control.Monad.IO.Class


checkStateAndRunAI :: (AIMonad m, AIDecider d) => d -> m ()
checkStateAndRunAI ai = do
    gameState <- getGameState
    case gameState of
        InGame side -> runAI ai side >>= executeMove
        _ -> putAppState (AppError "Not in game")


handleCommand :: (MonadAppState m, MonadGameState m, MonadBoard' m, MonadIO m) => String -> m ()
handleCommand "quit" = putAppState End
handleCommand "ai random" = checkStateAndRunAI RandomDecision 
handleCommand "reset board" = putInitialGameBoard >> putAppState (OperationSuccessful "Board has been reset")
handleCommand "begin game" = putGameState (InGame Red) >> putAppState (OperationSuccessful "Game has begun")
handleCommand "help" = putAppState (OperationSuccessful $ concat $ intersperse "\n"
        ["Available Command: "
        ,"begin game"
        ,"reset board"
        , "ai random"
        , "quit"
        ])
handleCommand cmd = putAppState (AppError $ "Unrecognized Command: " ++ cmd ++ "; type help to show commands")
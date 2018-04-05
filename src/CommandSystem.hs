module CommandSystem where
    
import AppState
import CoreLib
import Data.List

handleCommand :: (MonadAppState m, MonadGameState m, MonadBoard m) => String -> m ()
handleCommand "quit" = putAppState End
handleCommand "reset board" = putInitialGameBoard >> putAppState (OperationSuccessful "Board has been reset")
handleCommand "begin game" = putGameState (InGame Red) >> putAppState (OperationSuccessful "Game has begun")
handleCommand "help" = putAppState (OperationSuccessful $ concat $ intersperse "\n"
        ["Available Command: "
        ,"begin game"
        ,"reset board"
        , "quit"
        ])
handleCommand cmd = putAppState (AppError $ "Unrecognized Command: " ++ cmd ++ "; type help to show commands")
module AIHandlerLogic where

import AI.AIInterface
import AI.RandomDecision
import AI.MiniMax
import AI.AIAlgorithm
import AppState
import GameLogic
import ApplicationMonads
import Control.Monad

class RunnableHandler h where
    isRunnable :: (AIMonad m) => h -> m Bool
    runHandler :: (AIMonad m) => h -> m ()

instance RunnableHandler Handler where
    isRunnable (AIHandler _ side) = 
        getGameState >>= \case
            (InGame currentSide) -> return $ side == currentSide
            _ -> return False -- handler is not runnable if game is finished
    runHandler (AIHandler algorithm _) = checkStateAndRunAI algorithm




checkStateAndRunAI :: (AIMonad m, AIDecider d) => d -> m ()
checkStateAndRunAI ai = do
        -- save cursor position
        cursor <- getPosition
        gameState <- getGameState
        case gameState of
                InGame side -> runAI ai side >>= executeMove True
                _ -> putAppState (AppError "Not in game")
        putPosition cursor

runCustomHandler :: (MonadCustomHandlers' m, AIMonad m) => m ()
runCustomHandler = do
    handlers <- getCustomHandlers
    runnables <- filterM isRunnable handlers
    mapM_ runHandler runnables
    if null runnables  -- check no eligible runnables left, if so return, else recurse (usually no runnables if game is finished)
        then return ()
        else runCustomHandler 
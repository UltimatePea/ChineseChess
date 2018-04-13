{-# LANGUAGE ConstraintKinds #-}
module AI.AIInterface where

import ChineseChess.CoreLib
import AppState
import ChineseChess.GameLogic
import ApplicationMonads
import Control.Monad.IO.Class


type AIMonad m = (MonadBoard' m, MonadGameState' m, MonadAppState' m, MonadIO m, MonadPosition' m)



class AIDecider a where
    runAI :: (AIMonad m) => a -> PieceSide -> m ((Int, Int), (Int, Int)) 



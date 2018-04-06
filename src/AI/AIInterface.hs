{-# LANGUAGE ConstraintKinds #-}
module AI.AIInterface where

import CoreLib
import AppState
import GameLogic
import ApplicationMonads
import Control.Monad.IO.Class


type AIMonad m = (MonadBoard m, MonadGameState m, MonadAppState m, MonadIO m, MonadPosition m, MonadRootStore m)

class AIDecider a where
    runAI :: (AIMonad m) => a -> PieceSide -> m ((Int, Int), (Int, Int)) 



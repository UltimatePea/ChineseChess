module AI.RandomDecision where

import System.Random
import AppState
import AI.Decisions
import AI.AIInterface
import Control.Monad.IO.Class

data RandomDecision = RandomDecision


instance AIDecider RandomDecision where
    runAI RandomDecision = randomDecision 

randomDecision ::  (AIMonad m) => PieceSide ->  m ((Int, Int), (Int, Int))
randomDecision side = do
    choices <- allMovablePieces side
    idx <- liftIO $ randomRIO (0, length choices - 1)
    return (choices !! idx)



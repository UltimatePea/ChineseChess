module AI.RandomDecision where

import System.Random
import AppState
import AI.Decisions
import AI.AIInterface
import Control.Monad.IO.Class




randomDecision ::  (AIMonad m) => PieceSide ->  m ((Int, Int), (Int, Int))
randomDecision side = do
    choices <- allViableMoves side
    idx <- liftIO $ randomRIO (0, length choices - 1)
    return (choices !! idx)



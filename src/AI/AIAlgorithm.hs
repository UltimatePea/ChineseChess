module AI.AIAlgorithm where

import AI.AIInterface
import AI.MiniMax
import AI.RandomDecision
import AppState


instance AIDecider AIAlgorithm where
    runAI RandomDecision side = randomDecision side
    runAI MiniMax side = runMiniMax minimaxDefaultDepth side >>= (\(_, m) -> return m)
    runAI MiniMaxWithAlphaBeta side = runMiniMaxAlphaBeta (-10000000) (10000000) 3 side >>= (\(_, m) -> return m)

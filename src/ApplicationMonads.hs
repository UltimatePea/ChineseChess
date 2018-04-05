{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module ApplicationMonads where
import AppState

class Monad m => MonadMovePieceAction m where
    getMoveFrom :: m (Int, Int)
    getMoveTo :: m (Int, Int)
    putMoveAction :: (Int, Int) -> (Int, Int) -> m ()

    -- WARNING: ARCHITECTURAL WARNING: move action should only be set when the user is moving, therefore should not appear in root store
instance (Monad m, MonadRootStore m) => MonadMovePieceAction m where
    getMoveFrom = do
        store <- getRootStore
        let (from , _) = movementAction store
        return from
    getMoveTo = do
        store <- getRootStore
        let (_ , to) = movementAction store
        return to
    putMoveAction from to = do
        store <- getRootStore
        putRootStore (store {movementAction = (from, to)})

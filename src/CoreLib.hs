{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CoreLib  where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Class


data PieceType = General | Advisor | Elephant | Horse | Chariot | Cannon | Soldier | Empty 
data PieceSide = Red | Black 
data Piece = Piece PieceSide PieceType

data RawBoard a = RawBoard [[a]]
type Board = RawBoard Piece

-- lens helper functions

setN :: Int -> a -> [a] -> [a]
setN n x xs = take n xs ++ [x] ++ drop (n+1) xs

setN2 :: Int -> Int -> a -> [[a]] -> [[a]]
setN2 y x t xss = setN y (setN x t (xss !! y)) xss

class Monad m => MonadBoard m where
    getBoard :: m Board
    putBoard :: Board -> m ()

instance Monad m => MonadBoard (StateT Board m) where
    getBoard = get
    putBoard = put

instance {-# OVERLAPPABLE #-} (MonadBoard m, MonadTrans t, Monad (t m)) => MonadBoard (t m) where
    getBoard = lift getBoard
    putBoard = lift . putBoard

putEmptyBoard :: (MonadBoard m) => m ()
putEmptyBoard = putBoard $ RawBoard (replicate 10 (replicate 9 (Piece Red Empty)))

updatePiece :: MonadBoard m => Int -> Int -> Piece -> m ()
updatePiece y x p =  do
    (RawBoard xs) <- getBoard
    putBoard (RawBoard (setN2 y x p xs))

putInitialGameBoard :: MonadBoard m => m ()
putInitialGameBoard = do
    putEmptyBoard
    updatePiece 0 0 (Piece Black Chariot)
    updatePiece 0 1 (Piece Black Horse)
    updatePiece 0 2 (Piece Black Elephant)
    updatePiece 0 3 (Piece Black Advisor)
    updatePiece 0 4 (Piece Black General)
    updatePiece 0 5 (Piece Black Advisor)
    updatePiece 0 6 (Piece Black Elephant)
    updatePiece 0 7 (Piece Black Horse)
    updatePiece 0 8 (Piece Black Chariot)

    updatePiece 2 1 (Piece Black Cannon)
    updatePiece 2 7 (Piece Black Cannon)

    updatePiece 3 0 (Piece Black Soldier)
    updatePiece 3 2 (Piece Black Soldier)
    updatePiece 3 4 (Piece Black Soldier)
    updatePiece 3 6 (Piece Black Soldier)
    updatePiece 3 8 (Piece Black Soldier)

    updatePiece 6 0 (Piece Red Soldier)
    updatePiece 6 2 (Piece Red Soldier)
    updatePiece 6 4 (Piece Red Soldier)
    updatePiece 6 6 (Piece Red Soldier)
    updatePiece 6 8 (Piece Red Soldier)

    updatePiece 7 1 (Piece Red Cannon)
    updatePiece 7 7 (Piece Red Cannon)

    updatePiece 9 0 (Piece Red Chariot)
    updatePiece 9 1 (Piece Red Horse)
    updatePiece 9 2 (Piece Red Elephant)
    updatePiece 9 3 (Piece Red Advisor)
    updatePiece 9 4 (Piece Red General)
    updatePiece 9 5 (Piece Red Advisor)
    updatePiece 9 6 (Piece Red Elephant)
    updatePiece 9 7 (Piece Red Horse)
    updatePiece 9 8 (Piece Red Chariot)

newGameBoard :: Board
newGameBoard = execState putInitialGameBoard (RawBoard [])

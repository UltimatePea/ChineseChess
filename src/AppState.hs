module AppState where



data PieceType = General | Advisor | Elephant | Horse | Chariot | Cannon | Soldier | Empty 
data PieceSide = Red | Black 
data Piece = Piece PieceSide PieceType

data RawBoard a = RawBoard [[a]]
type Board = RawBoard Piece

data RootStore = RootStore
    {
          cursorPosition :: (Int, Int)
        , board :: Board
    }
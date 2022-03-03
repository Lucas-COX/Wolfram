module Cell where

data CellState = Alive | Dead deriving Eq

instance Show CellState where
    show a = if a == Alive then "*" else "o"

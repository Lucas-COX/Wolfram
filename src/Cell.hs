module Cell (
    CellState (..)
) where

data CellState = Alive | Dead deriving Eq

instance Show CellState where
    show Alive = "*"
    show Dead = " "

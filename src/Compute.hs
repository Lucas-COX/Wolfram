module Compute (
    computeNext
) where

import Cell ( CellState (..) )
import Data.Word (Word8)
import Data.Maybe ( fromJust )
import Universe ( getNeighborhood, U (..) )
import Utils ( getNBit )
import WConfig ( Config (..) )


computeRule :: Word8 -> [CellState] -> CellState
computeRule n [Alive, Alive, Alive] = if getNBit n 7 then Alive else Dead
computeRule n [Alive, Alive, Dead] = if getNBit n 6 then Alive else Dead
computeRule n [Alive, Dead, Alive] = if getNBit n 5 then Alive else Dead
computeRule n [Alive, Dead, Dead] = if getNBit n 4 then Alive else Dead
computeRule n [Dead, Alive, Alive] = if getNBit n 3 then Alive else Dead
computeRule n [Dead, Alive, Dead] = if getNBit n 2 then Alive else Dead
computeRule n [Dead, Dead, Alive] = if getNBit n 1 then Alive else Dead
computeRule n [Dead, Dead, Dead] = if odd n then Alive else Dead
computeRule _ _ = Dead


computeLeftList :: Word8 -> CellState -> [CellState] -> [CellState]
computeLeftList ru x (l : h : ls) =
  computeRule ru [h, l, x] : computeLeftList ru l (h : ls)
computeLeftList _ _ ls = ls


computeRightList :: Word8 -> CellState -> [CellState] -> [CellState]
computeRightList ru x (l : h : ls) =
  computeRule ru [x, l, h] : computeRightList ru l (h : ls)
computeRightList _ _ ls = ls


computeNext :: Config -> U CellState -> U CellState
computeNext _ (U [] x rs) = U [] x rs
computeNext _ (U ls x []) = U ls x []
computeNext c (U ls x rs) =
  U
    (computeLeftList (Data.Maybe.fromJust (rule c)) x ls)
    (computeRule (Data.Maybe.fromJust (rule c)) (getNeighborhood (U ls x rs)))
    (computeRightList (Data.Maybe.fromJust (rule c)) x rs)

module Wolfram (
  wolfram,
) where

import Cell (CellState (..))
import Compute ( computeNext )
import Data.Maybe ( fromJust )
import Data.Word ( Word8 )
import Universe (
    uToString,
    U(..),
    getNeighborhood,
    getNCells,
    shift
  )
import Utils ( reverseList )
import WConfig (Config ( lineNb, move, rule, start, window) )


createUniverse :: U CellState
createUniverse = U (repeat Dead) Alive (repeat Dead)


printLoop :: Config -> U CellState -> IO ()
printLoop c u
  | Data.Maybe.fromJust (lineNb c) == -1 = putStrLn (uToString $ getNCells
    (Data.Maybe.fromJust (window c)) u) >> printLoop c (computeNext c u)
  | Data.Maybe.fromJust (lineNb c) > 1 =
    putStrLn (uToString $ getNCells (Data.Maybe.fromJust (window c)) u)
    >> printLoop (c {lineNb = Just (Data.Maybe.fromJust (lineNb c) - 1)
    }) (computeNext c u)
  | Data.Maybe.fromJust (lineNb c) == 1
    = putStrLn (uToString $ getNCells (Data.Maybe.fromJust (window c)) u)
  | otherwise = putStr ""


startLoop :: Config -> U CellState -> U CellState
startLoop c u
  | Data.Maybe.fromJust (start c) > 0 = startLoop c {
      start = Just (Data.Maybe.fromJust (start c) - 1)
    } (computeNext c u)
  | otherwise = u


applyMove :: Config -> U CellState -> U CellState
applyMove c u =
  if Data.Maybe.fromJust (move c) == 0
    then u
    else shift u (Data.Maybe.fromJust (move c))


wolfram :: Config -> IO ()
wolfram c = printLoop c $ startLoop c $ applyMove c createUniverse

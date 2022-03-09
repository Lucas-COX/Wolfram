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
  | fromJust (lineNb c) == -1 = putStrLn (uToString $ getNCells
    (fromJust (window c)) u) >> printLoop c (computeNext c u)
  | fromJust (lineNb c) > 1 =
    putStrLn (uToString $ getNCells (fromJust (window c)) u)
    >> printLoop (c {lineNb = Just (fromJust (lineNb c) - 1)})
    (computeNext c u)
  | fromJust (lineNb c) == 1
    = putStrLn (uToString $ getNCells (fromJust (window c)) u)
  | otherwise = putStr ""


startLoop :: Config -> U CellState -> U CellState
startLoop c u
  | fromJust (start c) > 0 = startLoop c {
      start = Just (fromJust (start c) - 1)
    } (computeNext c u)
  | otherwise = u


applyMove :: Config -> U CellState -> U CellState
applyMove c u =
  if fromJust (move c) == 0
    then u
    else shift u (fromJust (move c))


wolfram :: Config -> IO ()
wolfram c = printLoop c $ startLoop c $ applyMove c createUniverse

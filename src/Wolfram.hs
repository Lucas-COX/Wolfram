module Wolfram (
  wolfram,
) where

import WConfig ( Config(rule, lineNb, window, start, move) )
import Cell ( CellState(..) )
import Universe (
    uToString,
    U(..),
    getNeighborhood,
    getNCells,
    shift
  )
import Utils ( reverseList )

import Data.Maybe ( fromJust )
import Data.Word ( Word8 )


createUniverse :: U CellState
createUniverse = U (repeat Dead) Alive (repeat Dead)


addToByte :: Word8 -> Int -> [Bool]
addToByte b n
  | n < 0 = []
  | b >= 2^n = True:addToByte (b - 2^n) (n - 1)
  | b < 2^n = False:addToByte b (n - 1)
  | otherwise = []


createByte :: Word8 -> [Bool]
createByte n = addToByte n 7


getNBit :: Word8 -> Word8 -> Bool
getNBit w n = reverseList (createByte w)!!(fromIntegral n :: Int)


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
computeLeftList ru x (l:h:ls) =
  computeRule ru [h, l, x]:computeLeftList ru l (h:ls)
computeLeftList _ _ ls = ls


computeRightList :: Word8 -> CellState -> [CellState] -> [CellState]
computeRightList ru x (l:h:ls) =
  computeRule ru [x, l, h]:computeRightList ru l (h:ls)
computeRightList _ _ ls = ls


computeNext :: Config -> U CellState -> U CellState
computeNext _ (U [] x rs ) = U [] x rs
computeNext _ (U ls x []) = U ls x []
computeNext c (U ls x rs) = U
    (computeLeftList (Data.Maybe.fromJust (rule c)) x ls)
    (computeRule (Data.Maybe.fromJust (rule c)) (getNeighborhood (U ls x rs)))
    (computeRightList (Data.Maybe.fromJust (rule c)) x rs)


printLoop :: Config -> U CellState -> IO ()
printLoop c u
  | Data.Maybe.fromJust (lineNb c) == -1 =
    putStrLn (uToString $ getNCells (Data.Maybe.fromJust (window c)) u)
    >> printLoop c (computeNext c u)
  | Data.Maybe.fromJust (lineNb c) > 1 =
    putStrLn (uToString $ getNCells (Data.Maybe.fromJust (window c)) u)
    >> printLoop (c {
      lineNb = Just (Data.Maybe.fromJust (lineNb c) - 1)
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

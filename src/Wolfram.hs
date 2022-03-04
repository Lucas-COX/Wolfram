module Wolfram where

import WConfig ( Config(rule, lineNb, window) )
import Cell ( CellState(..) )
import Universe ( uToString, U(..), lshift, rshift, llist, rlist, getNeighborhood, uToList )
import Utils ( maybeSub )

import qualified Data.Maybe
import Control.Applicative (liftA2)
import Data.Word (Word8)

createUniverse :: Maybe Int -> U CellState
createUniverse (Just n) = if even n
    then U (replicate (n `div` 2) Dead) Alive (replicate (n `div` 2 - 1) Dead)
    else U (replicate (n `div` 2) Dead) Alive (replicate (n `div` 2) Dead)
createUniverse Nothing = U (replicate 40 Dead) Alive (replicate 39 Dead)

--todo format function to handle width and shift (use rotate on normal list)

--todo function that returns function that computes for all rules

getNBit :: Word8 -> Word8 -> Bool
getNBit w n = (w `div` 2^n) /= 0

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
computeLeftList ru x (l:h:ls) = computeRule ru [h, l, x]:computeLeftList ru l (h:ls)
computeLeftList _ _ ls = ls

computeRightList :: Word8 -> CellState -> [CellState] -> [CellState]
computeRightList ru x (l:h:ls) = computeRule ru [x, l, h]:computeRightList ru l (h:ls)
computeRightList _ _ ls = ls

-- todo to apply rule on left list we need to reverse it :c

computeNext :: Config -> U CellState -> U CellState
computeNext _ (U [] x rs ) = U [] x rs
computeNext _ (U ls x []) = U ls x []
computeNext c (U ls x rs) = U
    (computeLeftList (Data.Maybe.fromJust (rule c)) x ls)
    (computeRule (Data.Maybe.fromJust (rule c)) (getNeighborhood (U ls x rs)))
    (computeRightList (Data.Maybe.fromJust (rule c)) x rs)

printLoop :: Config -> U CellState -> IO ()
printLoop c u
  | Data.Maybe.isNothing (lineNb c) = putStrLn (uToString u)
      >> printLoop c (computeNext c u)
  | lineNb c > Just 0 = putStrLn (uToString u)
      >> printLoop (c {lineNb = maybeSub (lineNb c) (Just 1)}) (computeNext c u)
  | otherwise = putStrLn (uToString u)

wolfram :: Config -> IO ()
wolfram c = printLoop c $ createUniverse (window c)

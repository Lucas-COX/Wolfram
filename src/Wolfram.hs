module Wolfram where

import WConfig
import Cell
import Universe
import Utils

import qualified Data.Maybe
import Control.Applicative (liftA2)

createUniverse :: Maybe Int -> U CellState
createUniverse (Just n) = if even n
    then U (replicate (n `div` 2) Dead) Alive (replicate (n `div` 2 - 1) Dead)
    else U (replicate (n `div` 2) Dead) Alive (replicate (n `div` 2) Dead)
createUniverse Nothing = U (replicate 40 Dead) Alive (replicate 39 Dead)

computeNext :: U CellState -> U CellState
computeNext u = Data.Maybe.fromMaybe u (rshift u)

--todo format function to handle width and shift (use rotate on normal list)

--todo function that returns function that computes for all rules

printLoop :: Config -> U CellState -> IO ()
printLoop c u
  | Data.Maybe.isNothing (line_nb c) = putStrLn (uToString u)
    >> printLoop c (computeNext u)
  | line_nb c > Just 0 = putStrLn (uToString u) >> print u
    >> printLoop (c {line_nb = maybeSub (line_nb c) (Just 1)}) (computeNext u)
  | otherwise = putStrLn (uToString u)

wolfram :: Config -> IO ()
wolfram c = printLoop c $ createUniverse (window c)

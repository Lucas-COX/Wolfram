module Utils where

import WConfig
import Data.Maybe (isNothing)
import System.Exit (exitWith, ExitCode (ExitFailure))

displayUsage :: IO ()
displayUsage = readFile ".usage" >>= putStrLn

exitError :: Config -> IO ()
exitError c
  | isNothing (rule c) =
    putStrLn "wolfram: Invalid rule"
    >> displayUsage >> exitWith (ExitFailure 84)
  | isNothing (start c) =
    putStrLn "wolfram: Invalid start"
    >> displayUsage >> exitWith (ExitFailure 84)
  | isNothing (lineNb c) =
    putStrLn "wolfram: Invalid lines"
    >> displayUsage >> exitWith (ExitFailure 84)
  | isNothing (window c) =
    putStrLn "wolfram: Invalid window"
    >> displayUsage >> exitWith (ExitFailure 84)
  | isNothing (move c) =
    putStrLn "wolfram: Invlaid move"
    >> displayUsage >> exitWith (ExitFailure 84)
  | otherwise =
    putStrLn "wolfram: Bad arguments"
    >> displayUsage >> exitWith (ExitFailure 84)

reverseList :: [x] -> [x]
reverseList = foldl (flip(:)) []

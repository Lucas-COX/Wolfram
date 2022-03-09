module Utils (
    concatStrings,
    exitError,
    reverseList
) where

import WConfig ( Config(rule, start, lineNb, window, move) )
import Data.Maybe ( isNothing )
import System.Exit ( exitWith, ExitCode (ExitFailure))


concatStrings :: [String] -> String
concatStrings (a : as) = foldl (++) a as
concatStrings [] = ""


displayUsage :: IO ()
displayUsage = readFile ".usage" >>= putStrLn


exitError :: Config -> IO ()
exitError c
  | isNothing (rule c) = putStrLn "wolfram: Invalid rule"
    >> displayUsage >> exitWith (ExitFailure 84)
  | isNothing (start c) = putStrLn "wolfram: Invalid start"
    >> displayUsage >> exitWith (ExitFailure 84)
  | isNothing (lineNb c) = putStrLn "wolfram: Invalid lines"
    >> displayUsage >> exitWith (ExitFailure 84)
  | isNothing (window c) = putStrLn "wolfram: Invalid window"
    >> displayUsage >> exitWith (ExitFailure 84)
  | isNothing (move c) = putStrLn "wolfram: Invalid move"
    >> displayUsage >> exitWith (ExitFailure 84)
exitError c = putStrLn "wolfram: Bad arguments"
    >> displayUsage >> exitWith (ExitFailure 84)


reverseList :: [x] -> [x]
reverseList = foldl (flip(:)) []

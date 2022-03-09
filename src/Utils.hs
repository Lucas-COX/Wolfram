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


exitUsage :: IO ()
exitUsage = readFile ".usage" >>= putStrLn >> exitWith (ExitFailure 84)


exitError :: Config -> IO ()
exitError c
  | isNothing (rule c) = putStrLn "wolfram: Invalid rule" >> exitUsage
  | isNothing (start c) = putStrLn "wolfram: Invalid start" >> exitUsage
  | isNothing (lineNb c) = putStrLn "wolfram: Invalid lines" >> exitUsage
  | isNothing (window c) = putStrLn "wolfram: Invalid window" >> exitUsage
  | isNothing (move c) = putStrLn "wolfram: Invalid move" >> exitUsage
  | otherwise = putStrLn "wolfram: Bad arguments" >> exitUsage


reverseList :: [x] -> [x]
reverseList = foldl (flip(:)) []

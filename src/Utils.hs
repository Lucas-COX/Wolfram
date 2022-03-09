module Utils (
    concatStrings,
    exitError,
    getNBit,
    reverseList
) where

import WConfig ( Config(rule, start, lineNb, window, move) )
import Data.Maybe ( isNothing )
import Data.Word ( Word8 )
import System.Exit ( exitWith, ExitCode (ExitFailure))


addToByte :: Word8 -> Int -> [Bool]
addToByte b n
  | n < 0 = []
  | b >= 2 ^ n = True : addToByte (b - 2 ^ n) (n - 1)
  | b < 2 ^ n = False : addToByte b (n - 1)
  | otherwise = []


createByte :: Word8 -> [Bool]
createByte n = addToByte n 7


getNBit :: Word8 -> Word8 -> Bool
getNBit w n = reverseList (createByte w) !! (fromIntegral n :: Int)


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

module Main where

import GetOpts
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))


main :: IO ()
main = getArgs >>= print . show . getOpts defaultConf

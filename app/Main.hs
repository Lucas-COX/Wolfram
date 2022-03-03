module Main where

import WConfig ( defaultConf, getOpts )
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Wolfram (wolfram)

main :: IO ()
main = getArgs >>= (\args -> case getOpts defaultConf args of
        Just opts -> wolfram opts
        Nothing -> exitWith (ExitFailure 84)
    )

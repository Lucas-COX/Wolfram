module Main where

import WConfig ( defaultConf, getOpts, Config (rule) )
import Wolfram (wolfram)

import Data.Maybe ( isNothing )
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))

main :: IO ()
main = getArgs >>= (\args -> case getOpts defaultConf args of
        Just opts -> if isNothing (rule opts)
            then exitWith (ExitFailure 84)
            else wolfram opts
        Nothing -> exitWith (ExitFailure 84)
    )

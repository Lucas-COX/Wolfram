module Main where

import WConfig (
    defaultConf,
    getOpts,
    Config (rule, start, lineNb, window, move),
    checkConfig
  )
import Utils ( exitError )
import Wolfram ( wolfram )

import Data.Maybe ( isNothing )
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode (ExitFailure) )

main :: IO ()
main = getArgs >>= (\args -> if checkConfig $ getOpts defaultConf args
      then wolfram $ getOpts defaultConf args
      else exitError $ getOpts defaultConf args
    )

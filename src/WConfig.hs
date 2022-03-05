module WConfig where

import Text.Read (readMaybe)
import Data.Maybe ( isNothing )
import Data.Word (Word8)

data Config =
    Config {
    rule :: Maybe Word8
    , start :: Maybe Int
    , lineNb :: Maybe Int
    , window :: Maybe Int
    , move :: Maybe Int
    } deriving Show


defaultConf :: Config
defaultConf = Config {
    rule = Nothing
    , start = Just 0
    , lineNb = Just (-1)
    , window = Just 80
    , move = Just 0
    }


parseRule :: Maybe Int -> Maybe Word8
parseRule Nothing = Nothing
parseRule (Just x)
  | x <= 255 && x >= 0 = Just (fromIntegral x :: Word8)
  | otherwise = Nothing


parseUInt :: Maybe Int -> Maybe Int
parseUInt Nothing = Nothing
parseUInt (Just x)
  | x < 0 = Nothing
  | otherwise = Just x

checkConfig :: Config -> Bool
checkConfig c
  | isNothing (rule c) = False
  | isNothing (lineNb c) = False
  | isNothing (start c) = False
  | isNothing (window c) = False
  | isNothing (move c) = False
  | otherwise = True

getOpts :: Config -> [String] -> Config
getOpts c ("--rule":value:rest) =
        getOpts (c {rule = parseRule (readMaybe value :: Maybe Int)}) rest
getOpts c ("--start":value:rest) =
        getOpts (c {start = parseUInt (readMaybe value :: Maybe Int)}) rest
getOpts c ("--lines":value:rest) =
        getOpts (c {lineNb = parseUInt (readMaybe value) :: Maybe Int}) rest
getOpts c ("--window":value:rest) =
        getOpts (c {window = parseUInt (readMaybe value) :: Maybe Int}) rest
getOpts c ("--move":value:rest) =
        getOpts (c {move = readMaybe value}) rest
getOpts c _ = c

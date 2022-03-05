module WConfig where

import Text.Read (readMaybe)
import Data.Word (Word8)

data Config =
    Config {
    rule :: Maybe Word8
    , start :: Int
    , lineNb :: Int
    , window :: Int
    , move :: Int
    } deriving Show


defaultConf :: Config
defaultConf = Config {
    rule = Nothing
    , start = 0
    , lineNb = -1
    , window = 80
    , move = 0
    }


parseRule :: Config -> [String] -> Int -> Maybe Config
parseRule c rest x
  | x <= 255 && x >= 0
    = getOpts (c {rule = Just (fromIntegral x :: Word8)}) rest
  | otherwise = Nothing

getOpts :: Config -> [String] -> Maybe Config
getOpts c ("--rule":value:rest) = case (readMaybe value :: Maybe Int) of
        Just x -> parseRule c rest x
        Nothing -> Nothing
getOpts c ("--start":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {start = x}) rest
        Nothing -> Nothing
getOpts c ("--lines":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {lineNb = x}) rest
        Nothing -> Nothing
getOpts c ("--window":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {window = x}) rest
        Nothing -> Nothing
getOpts c ("--move":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {move = x}) rest
        Nothing -> Nothing
getOpts c [] = Just c
getOpts _ (a:_) = Nothing

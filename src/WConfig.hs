module WConfig where

import Text.Read (readMaybe)
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
    , start = Nothing
    , lineNb = Nothing
    , window = Nothing
    , move = Nothing
    }


getOpts :: Config -> [String] -> Maybe Config
getOpts c ("--rule":value:rest) = case (readMaybe value :: Maybe Word8) of
        Just x -> getOpts (c {rule = Just x}) rest
        Nothing -> Nothing
getOpts c ("--start":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {start = Just x}) rest
        Nothing -> Nothing
getOpts c ("--lines":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {lineNb = Just x}) rest
        Nothing -> Nothing
getOpts c ("--window":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {window = Just x}) rest
        Nothing -> Nothing
getOpts c ("--move":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {window = Just x}) rest
        Nothing -> Nothing
getOpts c [] = Just c
getOpts _ (a:_) = Nothing

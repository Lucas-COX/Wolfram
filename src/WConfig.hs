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
    , start = Just 0
    , lineNb = Just (-1)
    , window = Just 80
    , move = Just 0
    }


parseRule :: Int -> Maybe Word8
parseRule x
  | x <= 255 && x >= 0 = Just (fromIntegral x :: Word8)
  | otherwise = Nothing


parseUInt :: Int -> Maybe Int
parseUInt x
  | x < 0 = Nothing
  | otherwise = Just x

getOpts :: Config -> [String] -> Maybe Config
getOpts c ("--rule":value:rest) = case (readMaybe value :: Maybe Int) of
        Just x -> getOpts (c {rule = parseRule x}) rest
        Nothing -> Nothing
getOpts c ("--start":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts  (c {start = parseUInt x}) rest
        Nothing -> Nothing
getOpts c ("--lines":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {lineNb = parseUInt x}) rest
        Nothing -> Nothing
getOpts c ("--window":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {window = parseUInt x}) rest
        Nothing -> Nothing
getOpts c ("--move":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {move = parseUInt x}) rest
        Nothing -> Nothing
getOpts c [] = Just c
getOpts _ (a:_) = Nothing

module GetOpts where

import Text.Read (readMaybe)

data Config =
    Config {
    rule :: Int
    , start :: Maybe Int
    , line_nb :: Maybe Int
    , window :: Maybe Int
    , move :: Maybe Int
    } deriving Show


defaultConf :: Config
defaultConf = Config {
    rule = -1
    , start = Nothing
    , line_nb = Nothing
    , window = Nothing
    , move = Nothing
    }


getOpts :: Config -> [String] -> Maybe Config
getOpts c ("--rule":value:rest) = case (readMaybe value :: Maybe Int) of
        Just x -> getOpts (c {rule = x}) rest
        Nothing -> Nothing
getOpts c ("--start":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {start = Just x}) rest
        Nothing -> Nothing
getOpts c ("--lines":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {line_nb = Just x}) rest
        Nothing -> Nothing
getOpts c ("--window":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {window = Just x}) rest
        Nothing -> Nothing
getOpts c ("--move":value:rest) = case readMaybe value :: Maybe Int of
        Just x -> getOpts (c {window = Just x}) rest
        Nothing -> Nothing
getOpts c _ = Just c

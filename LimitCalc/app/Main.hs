{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Web.Scotty
import Control.Monad
import Text.Read (readMaybe)
import System.Environment
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Prelude hiding (error, function)



main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then putStrLn "Usage: LimitCalc.exe <port>" else
        case readMaybe (head args) of
            Nothing -> putStrLn "The port must be a number"
            Just port ->
                if port < 0 || port > 65535 then putStrLn "Invalid port value" else scotty port server


data LimitRequest = LimitReq {
    function  :: String,
    point     :: String,
    isPrecise :: Maybe Bool
} deriving (Generic)

instance ToJSON LimitRequest where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }
instance FromJSON LimitRequest where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }

data Error = Error {
    message :: String,
    location :: Integer
} deriving (Generic)

instance ToJSON Error where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }
instance FromJSON Error where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }

data Limit = Limit {
    hasLimit :: Bool,
    value :: Maybe String
} deriving (Generic)

instance ToJSON Limit where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }
instance FromJSON Limit where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }

data LimitResponse = LimitResp {
    error :: Maybe Error,
    limit :: Maybe Limit
} deriving (Generic)

instance ToJSON LimitResponse where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }
instance FromJSON LimitResponse where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }

resp = LimitResp {error = Nothing, limit = Just Limit {hasLimit = False, value = Nothing}}

server :: ScottyM ()
server = do
    get "/" $ do
        file "./static/index.html"
    post "/api/limits" $ do
        (req :: LimitResponse) <- jsonData
        
        text "post ok"
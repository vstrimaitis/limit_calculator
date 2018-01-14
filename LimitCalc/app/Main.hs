{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Web.Scotty hiding (function)
import Network.HTTP.Types.Status
import Control.Monad
import Text.Read (readMaybe)
import System.Environment
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, genericToJSON, genericParseJSON, defaultOptions, omitNothingFields, toJSON, parseJSON)
import Prelude hiding (error, function)

import qualified LimitCalc.Parsing as P
import LimitCalc
import LimitCalc.Limits hiding (Limit)

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

data ErrorType = FunctionParse | PointParse | UnknownLimit | Other deriving (Generic)
instance ToJSON ErrorType
instance FromJSON ErrorType

data Error = Error {
    message  :: String,
    errType  :: Maybe ErrorType,
    location :: Maybe Integer
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

server :: ScottyM ()
server = do
    get "/" $ do
        file "./static/index.html"
    post "/api/limits" $ do
        (req :: LimitRequest) <- jsonData
        let expr = function req
        let pt = point req
        let isPrec = isPrecise req
        case isPrec of
            Just True -> handlePrecise expr pt
            _         -> handleInprecise expr pt

handlePrecise expr pt = do
    status status500
    text "Precise calculations are not supported yet"

handleInprecise exprStr ptStr = do
    let exprParseResult = P.parseExpr exprStr
    let ptParseResult = P.parsePoint ptStr
    case (exprParseResult, ptParseResult) of
        (Left err, _) -> json $ buildFunctionParseError err
        (_, Left err) -> json $ buildPointParseError err
        (Right expr, Right pt) -> do
            let lim = findLimit pt expr
            json $ buildLimitResponse lim



emptyResponse = LimitResp Nothing Nothing
emptyError = Error "" Nothing Nothing

buildFunctionParseError err = emptyResponse{ error = Just Error {message = P.message err, errType = Just FunctionParse, location = Just (P.position err)}}

buildPointParseError err = emptyResponse {error = Just Error {message = P.message err, errType = Just PointParse, location = Just (P.position err)}}

buildLimitResponse Unknown = emptyResponse {error = Just emptyError {message = "Unable to find the limit"}}
buildLimitResponse NoLimit = emptyResponse {limit = Just Limit {hasLimit = False, value = Nothing}}
buildLimitResponse (HasLimit PositiveInfinity) = emptyResponse {limit = Just Limit {hasLimit = True, value = Just "+inf"}}
buildLimitResponse (HasLimit NegativeInfinity) = emptyResponse {limit = Just Limit {hasLimit = True, value = Just "-inf"}}
buildLimitResponse (HasLimit (Finite lim)) = emptyResponse {limit = Just Limit {hasLimit = True, value = Just (show lim)}}

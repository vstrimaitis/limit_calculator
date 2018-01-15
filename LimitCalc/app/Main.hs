{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Web.Scotty hiding (function)
import Control.Monad
import Text.Read (readMaybe)
import System.Environment
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, genericToJSON, genericParseJSON, defaultOptions, omitNothingFields, toJSON, parseJSON)
import Prelude hiding (error, function)

import qualified LimitCalc.Parsing as P
import qualified LimitCalc
import LimitCalc.Point

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


data Result
    = OK
    | FunctionParseError
    | PointParseError
    | UnknownLimit
    | OutOfFuel
    | UnsupportedOperation
    | FunctionUndefined
    deriving (Generic)
instance ToJSON Result
instance FromJSON Result

data LimitResponse = LimitResp {
    result :: Result,
    errorMessage :: Maybe String,
    errorLocation :: Maybe Integer,
    hasLimit :: Maybe Bool,
    limit :: Maybe String
} deriving (Generic)

instance ToJSON LimitResponse where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }
instance FromJSON LimitResponse where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }

emptyResponse = LimitResp {result = OK, errorMessage = Nothing, errorLocation = Nothing, hasLimit = Nothing, limit = Nothing}

server :: ScottyM ()
server = do
    get "/" $ file "./static/index.html"
    post "/api/limits" $ do
        (req :: LimitRequest) <- jsonData
        let expr = function req
        let pt = point req
        let isPrec = isPrecise req
        case isPrec of
            Just True -> handlePrecise expr pt
            _         -> handleInprecise expr pt

handlePrecise expr pt = do
    json $ emptyResponse
        { result = UnsupportedOperation
        , errorMessage = Just "Precise calculations are not supported yet"
        }

handleInprecise exprStr ptStr = do
    let exprParseResult = P.parseExpr exprStr
    let ptParseResult = P.parsePoint ptStr
    case (exprParseResult, ptParseResult) of
        (Left err, _) -> json emptyResponse {result = FunctionParseError, errorMessage = Just (P.message err), errorLocation = Just (P.position err)}
        (_, Left err) -> json emptyResponse {result = PointParseError, errorMessage = Just (P.message err), errorLocation = Just (P.position err)}
        (Right expr, Right pt) -> do
            let lim = findLimit pt expr
            json $ buildLimitResponse lim


buildLimitResponse Undefined = emptyResponse {result = FunctionUndefined}
buildLimitResponse Unknown = emptyResponse {result = UnknownLimit}
buildLimitResponse NoLimit = emptyResponse {hasLimit = Just False}
buildLimitResponse OutOfFuel = emptyResponse {result = Main.OutOfFuel}
buildLimitResponse (HasLimit PositiveInfinity) = emptyResponse {hasLimit = Just True, limit = Just "+inf"}
buildLimitResponse (HasLimit NegativeInfinity) = emptyResponse {hasLimit = Just True, limit = Just "-inf"}
buildLimitResponse (HasLimit (Finite lim)) = emptyResponse {hasLimit = Just True, limit = Just (show lim)}

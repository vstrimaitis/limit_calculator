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
import Data.Ratio

import qualified LimitCalc.Parsing as P
import LimitCalc
import LimitCalc.Point
import LimitCalc.Expr (Expr, fromAst)
import qualified LimitCalc.Ast as Ast
import qualified LimitCalc.AstPoint as AstPt
import ShowLatex

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


data ResponseType
    = OK
    | FunctionParseError
    | PointParseError
    | UnknownLimit
    | RanOutOfFuel
    | UnsupportedOperation
    | FunctionUndefined
    deriving (Generic)
instance ToJSON ResponseType
instance FromJSON ResponseType

data LimitResponse = LimitResp
    { result :: ResponseType
    , errorMessage :: Maybe String
    , errorLocation :: Maybe Integer
    , hasLimit :: Maybe Bool
    , limit :: Maybe String
    , exprLatex :: Maybe String
    , pointLatex :: Maybe String
    , limitLatex :: Maybe String
    } deriving (Generic)

instance ToJSON LimitResponse where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }
instance FromJSON LimitResponse where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }

emptyResponse = LimitResp
    { result = OK
    , errorMessage = Nothing
    , errorLocation = Nothing
    , hasLimit = Nothing
    , limit = Nothing
    , exprLatex = Nothing
    , pointLatex = Nothing
    , limitLatex = Nothing
    }

server :: ScottyM ()
server = do
    get "/" $ file "./static/index.html"
    get "/script.js" $ file "./static/script.js"
    get "/style.css" $ file "./static/style.css"
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
    let exprParseResult :: Either P.ParseError (Ast.Expr Rational) = P.parseExpr exprStr
    let ptParseResult :: Either P.ParseError (Point (AstPt.Value Rational)) = P.parsePoint ptStr
    case (exprParseResult, ptParseResult) of
        (Left err, _) -> json emptyResponse {result = FunctionParseError, errorMessage = Just (P.message err), errorLocation = Just (P.position err)}
        (_, Left err) -> json emptyResponse {result = PointParseError, errorMessage = Just (P.message err), errorLocation = Just (P.position err)}
        (Right expr, Right pt) -> do
            let astWithDoubles = fmap fromRational expr :: Ast.Expr Double
            let ptWithDoubles = fmap (fmap fromRational) pt :: Point (AstPt.Value Double)
            let ptValue = fmap AstPt.foldToValue ptWithDoubles
            let lim = findLimit ptValue (fromAst expr)
            json $ buildLimitResponse astWithDoubles ptWithDoubles lim

makeLatex :: (ShowLatex a, ShowLatex b) => Ast.Expr a -> Point b -> String
makeLatex expr pt = "\\lim_{x \\to " ++ showLatex pt ++ "} " ++ showLatex expr

buildLimitResponse :: (ShowLatex a, ShowLatex b, ShowLatex c, Show c)
    => Ast.Expr a
    -> Point b
    -> Result c
    -> LimitResponse
buildLimitResponse expr pt Undefined = emptyResponse
    { result = FunctionUndefined
    , exprLatex = Just $ showLatex expr
    , pointLatex = Just $ showLatex pt
    }
buildLimitResponse expr pt Unknown = emptyResponse
    { result = UnknownLimit
    , exprLatex = Just $ showLatex expr
    , pointLatex = Just $ showLatex pt
    }
buildLimitResponse expr pt NoLimit = emptyResponse
    { hasLimit = Just False
    , exprLatex = Just $ showLatex expr
    , pointLatex = Just $ showLatex pt
    }
buildLimitResponse expr pt OutOfFuel = emptyResponse
    { result = RanOutOfFuel
    , exprLatex = Just $ showLatex expr
    , pointLatex = Just $ showLatex pt
    }
buildLimitResponse expr pt (HasLimit lim@PositiveInfinity) = emptyResponse
    { hasLimit = Just True
    , limit = Just "+inf"
    , exprLatex = Just $ showLatex expr
    , pointLatex = Just $ showLatex pt
    , limitLatex = Just $ showLatex lim
    }
buildLimitResponse expr pt (HasLimit lim@NegativeInfinity) = emptyResponse
    { hasLimit = Just True
    , limit = Just "-inf"
    , exprLatex = Just $ showLatex expr
    , pointLatex = Just $ showLatex pt
    , limitLatex = Just $ showLatex lim
    }
buildLimitResponse expr pt (HasLimit lim@(Finite value)) = emptyResponse
    { hasLimit = Just True
    , limit = Just $ show value
    , exprLatex = Just $ showLatex expr
    , pointLatex = Just $ showLatex pt
    , limitLatex = Just $ showLatex lim
    }

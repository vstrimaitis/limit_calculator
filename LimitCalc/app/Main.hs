{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Monad
import Text.Read (readMaybe)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then putStrLn "Usage: LimitCalc.exe <port>" else
        case readMaybe (head args) of
            Nothing -> putStrLn "The port must be a number"
            Just port ->
                if port < 0 || port > 65535 then putStrLn "Invalid port value" else scotty port server

server :: ScottyM ()
server = do
    get "/" $ do
        file "./static/index.html"
    post "/api/limits" $ do
        text "post ok"